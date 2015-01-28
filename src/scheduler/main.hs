-- Author: Markus Kusano
--
-- DPOR Scheduler
--
import Network.Socket (Socket, socket, Family(AF_UNIX), SocketType(Stream), bind, SockAddr(SockAddrUnix), listen, accept, sClose)
import Network.Socket.ByteString (recv, sendAll)
import Network (withSocketsDo)
import System.Directory (removeFile, doesFileExist)
import Control.Monad (liftM, when)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe()
import Data.Word (Word8)
import qualified Data.ByteString as B
import System.Environment(getArgs)
import System.Exit (exitFailure)
import System.Console.GetOpt
import Data.List
import System.Process

import Transition (Transition(..), byteStringToTrans, getTransTId, sameTId, sameTId2, isAssertFail, TId)

import State(State(..), TIdToSock, updateStateAfterExec, createInitState, prettyPrintState, isTIdMember, getTransSetTId, VectorClock, ClockVal, updateDoneSelAfterExec, disjointLockset, happensBefore, tIdSetInsTrans)

-- Location of UNIX socket 
sockPathStr :: String
sockPathStr = "/tmp/systest.sock"

-- Maximum number of threads (the maximum number of queued connections on the
-- socket)
maxThreads :: Int
maxThreads = 32

-- Value sent to program under test for permission
permitVal :: Word8
permitVal = 9

-- The state stack.
--
-- The head of the list is the most recent state
type StateStack = [State]

-- Add the thread ID of the passed transition to the enabled set of the passed
-- state
--addTransToEnabled :: State -> Transition -> State
--addTransToEnabled s t = 

-- Pretin the stack starting with the first state and ending with the last
-- state (this is a reverse of the actual stack order)
prettyPrintStack :: StateStack -> String
prettyPrintStack s = prettyPrintStackHelper (reverse s) [] 0

prettyPrintStackHelper :: StateStack -> String -> Int -> String
prettyPrintStackHelper (s:ss) acc i = 
  prettyPrintStackHelper ss newAcc (i + 1)
          where newAcc = acc ++ "State " ++ (show i) ++ ":\n" ++ (prettyPrintState s) ++ "\n"
prettyPrintStackHelper [] acc _ = acc

--execProcIfNotStandalone :: Options -> IO ()
--execProcIfNotStandalone o = case (programPath) o of
--  [] -> return ()
--  _ -> do 
--    _ <- runProcess ((programPath o) (programArgs o) Nothing Nothing Nothing Nothing Nothing)
--    return ()

execProcIfNotStandalone :: Options -> IO ()
execProcIfNotStandalone o = do
   _ <- runProcess path args Nothing Nothing Nothing Nothing Nothing
   return ()
     where path = programPath o
           args = programArgs o

-- Repeatedly select an arbitrary transition to execute until there are no more
-- transitions to execute. 
--
-- Returns a state stack containing all the states encountered. The most
-- recently visited state will be the head of the list.
--
performFreeRun :: StateStack -> Socket -> TIdToSock -> IO StateStack
performFreeRun [] _ _ = 
  error "empty statestack passed to performFreeRun"
performFreeRun ss sock tidToSock 
  | (Set.null (enabledSet (head ss))) 
      && (not $ Set.null (disabledSet (head ss))) = error $ "Deadlock detected: "  ++ prettyPrintState (head ss)
  | (Set.null (enabledSet (head ss))) 
      && (Set.null (disabledSet (head ss))) = return (ss)
  | otherwise = do
    let nextState = head ss
    -- Stop the free run when the state has no enabled transitions
    ((prevState, newState), newSocks) <- processStateFreeRun nextState sock tidToSock
    --putStrLn $ "New State:\n" ++ prettyPrintState newState
    --putStr "\n"
    -- Update the back track set and create a new state stack
    --let newSs = (newState:ss)
    -- remove the previous state (whihc was updated by processStateFreeRun).
    -- This is why we use tail on ss
    --
    -- UpdateBacktrackSet is not called on newState since newState has not had
    -- a transition selected from it yet
    let newSs = newState : updateBacktrackSet (prevState:(tail ss))
    performFreeRun newSs sock newSocks

-- One iteration of systematic concurrency testing for a free run
--
-- The process is as follows:
--
-- 1. Select an enabled transition (arbitrary, this is a free run)
-- 2. Create the new state reflecting the execution of this transition:
--    * Update vector clock 
--    * Update lock set (if necessary)
--    * Update disabled set (if necessary)
-- 3. Wait for the thread which just executed a transition to come back to the
-- socket
-- 4. Update the enabled set of the new state to reflect the current state of
-- the thread which just executed
--
-- Returns the newly created state
--
-- The passed socket is the socket accepting connections
processStateFreeRun :: State -> Socket -> TIdToSock -> IO ((State, State), TIdToSock)
processStateFreeRun st sock socks
  | Set.null (enabledSet st) 
    = error "State with empty enabled set passed to processStateFreeRun"
  | otherwise = do 
    -- Transition to execute from the passed state
    let sTrans = Set.findMin (enabledSet st)
    -- Since this is a new state, clear the done set of the previous state so
    -- its values do not carry over into the new state
    executeTrans (st {doneSet = Set.empty}) sTrans sock socks

-- Update the backtrack set of the head state in the passed state stack.
-- The head state is the state which was just executed. 
--
-- Let the transition which was just exected be selTrans.
--
-- This function scans the state stack from start to finish (excluding the head
-- state) to see if there exists a state s where:
--
-- selTrans is dependent with some transition in enabled(s)
-- selTrans does not happen before s
-- selTrans may be co-enabled with s
--
-- If all of these are true, then we attempt to add the thread which exected
-- selTrans to the backtrack set of the state pre(s) where pre(s) is the state
-- before s.
--
-- If thread(selTrans) is not enabled in pre(s) then we add the entire enabled
-- set to the backtrack set (over approximation)
updateBacktrackSet :: StateStack -> StateStack
updateBacktrackSet (s:ss) = 
  case split of
    (_, []) -> (s:ss)
    --(sts1, (st2:sts2)) -> s : sts1 ++ [st2] ++ (insertBacktrack (selTrans s) sts2)
    (sts1, sts2) -> s : sts1 ++ (insertBacktrack (selTrans s) sts2)
  -- Find a state which is dependent on the head state. If it does not exist,
  -- then we do not need to do any updating
  where split = splitOnDep s (ss)
updateBacktrackSet []  = []

--updateBacktrackSetNoDPOR :: StateStack -> StateStack
--updateBacktrackSetNoDPOR (s:ss) = 
--  case split of
--    (_, []) -> (s:ss)
--    --(sts1, (st2:sts2)) -> s : sts1 ++ [st2] ++ (insertBacktrack (selTrans s) sts2)
--    (sts1, sts2) -> s : sts1 ++ (insertBacktrack (selTrans s) sts2)
--  -- Find a state which is dependent on the head state. If it does not exist,
--  -- then we do not need to do any updating
--  where split = splitOnDepNoDPOR s (ss)
--updateBacktrackSetNoDPOR []  = []

-- Attempt to insert a backtrack point at the head state of the passed list.
--
-- This is an attempt since if the enabled set of the head state does not
-- contain the thread executing the passed transition then we over-approximate
-- the update by adding all the enabled threads to the backtrack set
insertBacktrack :: Transition -> StateStack -> StateStack
insertBacktrack (Invalid) _ = error "Selected transition is invalid"
insertBacktrack t (s:sts) = 
  -- Check if the passed transition is enabled in the head state
  if isTIdMember tTId (enabledSet s)
    -- Add the transition to the backtrack set if it is not already done
    then if Set.member tTId (doneSet s)
          -- No need to do it twice, just return the unmodified stack
          then (s:sts)
          -- Insert the enabled transition with the same TId to the backtrack
          -- set
          else (s { backtrackSet = Set.insert tTId (backtrackSet s) } : sts)
          -- Add the transition to the enabled set if it is not done
          --else (s { backtrackSet = Set.union (backtrackSet s) (getTransSetTId tTId (enabledSet s)) } : sts)
    -- If tTId is not found in the enabled set, then add the entire enabled set
    -- (which has not already been explored) to the backtrack set
    else s { backtrackSet = (backtrackSet s) 
      `Set.union` 
        (Set.map getTransTId ((enabledSet s))) 
        `Set.difference` 
        (doneSet s) } 
      : sts
  where tTId = getTransTId t
-- When the list is empty, this means updatebacktrack set found a dependent
-- transition but it is at the last state in the stack. In this case, we do not
-- need to do any up[dates since we cannot backtrack before the initial state
insertBacktrack _ [] = []

-- Split the state stack on the first state which has a selected transition
-- which is dependent and may be co-enabled with the passed state
--
-- If no such state is found, then the second member of the returned pair will
-- be empty
splitOnDep :: State -> StateStack -> (StateStack, StateStack)
splitOnDep _ [] = ([], []) :: (StateStack, StateStack)
splitOnDep selS s = splitOnDepHelper selS s []

splitOnDepHelper :: State -> StateStack -> StateStack -> (StateStack, StateStack)
splitOnDepHelper selS (st:sts) acc = 
  if depAndNotHB selS st
    then (acc, st:sts)
    --then error $ "TODO: splitOnDepHelper: " ++ "SelS:\n" ++ (prettyPrintState selS) ++ "\nAcc:\n" ++ (prettyPrintStack acc) ++ "\nRest:\n" ++ (prettyPrintStack (st:sts))
    --then error $ "TODO: st: " ++ (prettyPrintState st) ++ "\n"
    --then error $ "splitOnDepHelper: acc: " ++ (show (length acc))
    else splitOnDepHelper selS sts (acc ++ [st])
-- Called with empty state stack: we can't split nothing into something 
splitOnDepHelper _ [] _ = ([], [])

splitOnDepNoDPOR :: State -> StateStack -> (StateStack, StateStack)
splitOnDepNoDPOR _ [] = ([], []) :: (StateStack, StateStack)
splitOnDepNoDPOR selS s = splitOnDepHelperNoDPOR selS s []

splitOnDepHelperNoDPOR :: State -> StateStack -> StateStack -> (StateStack, StateStack)
splitOnDepHelperNoDPOR selS (st:sts) acc = 
  if depAndNotHBNoDPOR selS st
    then (acc, st:sts)
    --then error $ "TODO: splitOnDepHelper: " ++ "SelS:\n" ++ (prettyPrintState selS) ++ "\nAcc:\n" ++ (prettyPrintStack acc) ++ "\nRest:\n" ++ (prettyPrintStack (st:sts))
    --then error $ "TODO: st: " ++ (prettyPrintState st) ++ "\n"
    --then error $ "splitOnDepHelper: acc: " ++ (show (length acc))
    else splitOnDepHelper selS sts (acc ++ [st])
-- Called with empty state stack: we can't split nothing into something 
splitOnDepHelperNoDPOR _ [] _ = ([], [])

depAndNotHBNoDPOR :: State -> State -> Bool
depAndNotHBNoDPOR s1 s2 = 
  --case getDependent selT s2 of
  case dependentNoDPOR selT1 selT2 of
    False -> False
    _ -> case (not (happensBefore s2 selVC)) of
            True -> False -- s2 happens before s1
            -- If it does not happen before, then check if the locksets are
            -- disjoint
            False -> case disjointLockset s1 s2 of
                      True -> True
                      False -> False
  where selT1 = selTrans s1
        selT2 = selTrans s2
        --selVC = Map.! (vectorClockState s1) (getTransTId selT)
        selVC = Map.empty

-- depAndNotHB s1 s2
--
-- Returns true if the selected transition of s1 is dependent, does not
-- happen-before, and may be co-enabled with a transition in the enabled set of
-- s2
depAndNotHB :: State -> State -> Bool
depAndNotHB s1 s2 = 
  --case getDependent selT s2 of
  case dependent selT1 selT2 of
    False -> False
    _ -> case (not (happensBefore s2 selVC)) of
            True -> False -- s2 happens before s1
            -- If it does not happen before, then check if the locksets are
            -- disjoint
            False -> case disjointLockset s1 s2 of
                      True -> True
                      False -> False
  where selT1 = selTrans s1
        selT2 = selTrans s2
        --selVC = Map.! (vectorClockState s1) (getTransTId selT)
        selVC = Map.empty

-- Definition of dependence between two transitions: two transitions are
-- dependent iff:
--
-- 1. They are from two different threads
-- 2. Accessing the same memory location
-- 3. Atleast one is a write
dependent :: Transition -> Transition -> Bool                   
-- MutexLock calls are considered as writes to a mutex
dependent (MutLock t1 m1) (MutLock t2 m2) = (t1 /= t2) && (m1 == m2)
-- MutexDestroy is dependent on MutexLock
dependent (MutDestroy t1 m1) (MutLock t2 m2) = (t1 /= t2) && (m1 == m2)
dependent (MutLock t1 m1) (MutDestroy t2 m2) = (t1 /= t2) && (m1 == m2)
-- CondDestroy is dependent on CondWait and Broadcast
--
-- The value of the mutex is not important at this state: when a CondWait is
-- awoken by a CondSignal/Broadcast it need to acquire the lock afterwards (it
-- is converted to a MutLock transition)
dependent (CondDestroy t1 c1) (CondWait t2 c2 _) = (t1 /= t2) && (c1 == c2)
dependent (CondWait t1 c1 _) (CondDestroy t2 c2) = (t1 /= t2) && (c1 == c2)
dependent (CondDestroy t1 c1) (CondBroadcast t2 c2) = (t1 /= t2) && (c1 == c2)
dependent (CondBroadcast t1 c1) (CondDestroy t2 c2) = (t1 /= t2) && (c1 == c2)
-- A load/store or store/store to the same memory location by two different
-- threads
dependent (LoadPre t1 o1 _) (StorePre t2 o2 _) = (t1 /= t2) && (o1 == o2)
dependent (StorePre t1 o1 _) (LoadPre t2 o2 _) = (t1 /= t2) && (o1 == o2)
dependent (StorePre t1 o1 _) (StorePre t2 o2 _) = (t1 /= t2) && (o1 == o2)
-- Everything else is independent
dependent _ _ = False

-- When DPOR is disabled, two transitions are dependent if they are from two
-- different threads

dependentNoDPOR :: Transition -> Transition -> Bool
dependentNoDPOR t1 t2 = (getTransTId t1) /= (getTransTId t2)

-- Returns true if:
--
-- Returns false if the passed list of transitions contains a transitions from
-- the next thread.
--
-- This ensures that after a thread executes it reconnects with the scheduler.
--
-- The special cases are ThreadEnd and ThreadCreate:
--
-- For ThreadCreate, there should be one transitions from the parent and one
-- from the child
--
-- For ThreadEnd, the set of NextTransitions should be empty
--
--
-- This ensures that after permiting a thread to proceed it has returned with a
-- new transitions
invalidNextTrans :: Transition -> [Transition] -> Bool
-- For ThreadCreate, one transition should be from the parent thread and one
-- transition should be from the child thread
invalidNextTrans (ThreadCreate p c) ts = 
  -- Parent thread is not in next transitions
      (null . filter (sameTId2 p) $ ts)
  -- Or, child thread is not in the next transitions
  ||  (null $ filter (sameTId2 c) ts)
invalidNextTrans (ThreadEnd _) ts = not . null $ ts
invalidNextTrans t ts = null $ filter (sameTId t) ts

-- Wait for threads to send a new state over the socket. This is done after
-- permiting the passed event.
--
-- Returns a list of the next transitions for the threads based on the
-- execution.
--
-- There may be multiple next transitions if the passed transition to execute
-- is a ThreadCreate. The next transitions to execute will be the next
-- transition of the creator thread and the next transition of the child
-- thread.
--
-- There may be no (new) next transitions in the case of ThreadEnd. The thread
-- has exited and has no new transition to execute
--
-- Returns the list of next transitions and the updated TidToSock map
getNextTransitions :: Transition -> Socket -> TIdToSock -> IO ([Transition], TIdToSock)
-- For thread create, expect to recieve two transitions from two seperate
-- threads: One from the creator thread and one from the child thread. The
-- order does not matter
getNextTransitions (ThreadCreate _ _) sock socks = 
  withSocketsDo $ do
    -- Probably could have done this with an applicative functor
    (nt1, ns1) <- threadRecv sock socks
    (nt2, ns2) <- threadRecv sock ns1
    return ([nt1, nt2], ns2)

-- For thread end, the thread is exiting so there is no next transition
getNextTransitions (ThreadEnd _) _ socks = return ([], socks)

-- Otherwise, just expect one next transition after the permit
getNextTransitions _ sock socks = 
  withSocketsDo $ do
    (nextTrans, nextSocks) <- threadRecv sock socks
    return ([nextTrans], nextSocks)

-- Delete the socket file (if it exists)
cleanupSocket :: String -> IO ()
cleanupSocket f = do
  exists <- doesFileExist f
  if exists
    then removeFile f
    else return ()

-- Receive a transition from a thread. The passed socket is a bound and listening
-- socket. This function will add the thread and its accepted socket to the
-- passed tidToSock map if it is not in the map already.
threadRecv :: Socket -> TIdToSock -> IO (Transition, TIdToSock)
threadRecv lisSocket tidToSock = withSocketsDo $ do
    -- Receive data from the passed server socket. This returns a socket with
    -- communication to the thread
    
    -- Listen on the socket and get the socket to communicate with the thread
    sock <- acceptGetSock lisSocket

    -- recieve the transition via the socket
    msg <- recv sock 2048
    let tr = byteStringToTrans msg
    -- Insert the new socket into the map for the corresponding thread. If the
    -- socket thread already had a socket, it will be overwritten. This is what
    -- we want since we want to communicate on the most recent socket returned
    -- by accept
    let updatedMap = Map.insert (getTransTId tr) (sock) tidToSock
    -- TODO: Return the actual result
    return (tr, updatedMap)
    -- Accept returns (socket, sockaddr). For a unix socket, the socket is
    -- simply the path
    where acceptGetSock = liftM fst . accept

-- Send a Word8 == PermitValue (a global in this file) over the passed socket.
-- The socket is assumed to be connected and ready for data.
threadPermit :: Socket -> IO ()
threadPermit s = withSocketsDo $ do 
  sendAll s (B.singleton permitVal) 
  sClose s

-- Returns the prefix of the passed statestack.
--
-- This is the statestack with all the states after the state with the last
-- backtrack point removed. This is the first set of states for the next run.
getPrefix :: StateStack -> StateStack
-- Since the head of the list is the most recently executed state, we can
-- simply drop while the states have no backtrack points
getPrefix s = dropWhile (notHasBacktrack) $ s
  -- When passed a state, this returns true if the backtrack set is emtpy
  where notHasBacktrack = Set.null . backtrackSet

-- Continuously perform the following:
--
-- Given the passed prefix, replay it
-- Select the transition in the backtrack set of the prefix
-- Perform a free run until the program terminates
-- Calculate the next prefix
-- Repeat
--
-- The passed integer is a counter for the number of runs executed so far
explore :: Socket -> StateStack -> Options -> Int -> IO ()
-- We're done!
explore _ [] _ _ = return ()
-- The statestack starts from the most recently executed state. Reverse it so
-- we start from the begining of the run. The head of the passed list is the
-- state that we want to backtrack from
explore sock (st:sts) opts i = do
  --(replay sock) . (reverse sts)
  let revStack = reverse sts
  -- Replay the prefix. This will be everything up to the point where `st` was
  -- executed

  -- Startup the program
  execProcIfNotStandalone opts 

  (ss, socks) <- replay sock revStack
  when (verbose opts) $ putStrLn $ "\n====Replay Stack:\n" ++ prettyPrintStack ss

  when (null ss) $ error "Replay returned empty state stack"
  when (Set.null (backtrackSet st)) 
      $ error "Replaying to state with empty backtrack set"

  let btTrans = Set.findMin (backtrackSet st)

  --when (not $ Set.member btTrans (enabledSet . head $ ss))
  when (not $ isTIdMember btTrans (enabledSet . head $ ss))
    $ error ("Backtrack transition not member of current state" 
        ++ "\nReplay State:\n" ++ prettyPrintState st 
        ++ "\nNext State:\n" ++ prettyPrintState (head ss)  ++ "\n")

  -- Update the done set of the state where we are backtracking to to reflect
  -- previous executions. Also, clear the backtrack set.
  --let btState = st { doneSet = (doneSet st) `Set.union` (doneSet (head ss)), backtrackSet = Set.empty }
  let btState = st { backtrackSet = Set.empty }
  -- This will crash if the selected transition in the replay does not exist in
  -- the next state
  let nextT = getTransSetTId btTrans (enabledSet btState)

  -- Execute the backtrack transition
  --((prevState, nextState), newSocks) <- executeTrans (head ss) btTrans sock socks
  ((prevState, nextState), newSocks) <- executeTrans btState nextT sock socks
  -- Perform a free run starting from the state after the backtracked state.
  -- The resulting statestack is an entire run after executing the backtrack
  -- point
  when (null ss) $ error "explore: empty tail state stack"
  let newSs = nextState : (updateBacktrackSet (prevState : sts))
  --fullRun <- performFreeRun (nextState : prevState : (sts)) sock newSocks
  fullRun <- performFreeRun newSs sock newSocks
  when (verbose opts) $ 
      putStrLn $ "\nState stack of Run:\n" ++ prettyPrintStack fullRun
  putStrLn $ "====== Run " ++ (show i) ++ " ======"

  -- Compute the next prefix
  let prefix = getPrefix fullRun
  --putStrLn $ "Full Run:\n" ++ prettyPrintStack fullRun

  -- TODO: Do we need to do any cleanup of the TIdToSock map?

  -- Perform the next run
  explore sock prefix opts (i + 1)
  return ()

-- Starting from the head of the list, replay the selected transitions of the
-- passed StateStack. It is assumed that none of the threads are waiting on a
-- socket yet
--
-- Returns the statestack of the program after the replay is complete and the
-- current TIdToSock map
replay :: Socket -> StateStack -> IO (StateStack, TIdToSock)
--replay _ [] = return ([], Map.empty)
replay _ [] = error "replaying an empty state stack"
replay sock (s:sts) = do
  -- Get the first selected transition
  (firstTrans, tidToSock) <- threadRecv sock Map.empty
  let selT = selTrans s
  when (selT /= firstTrans) $ error "First transitions of replay do not match"

  -- During replay, we re-create the state stack of the replay trace to ensure
  -- that things match up. Also, this will be used to generate the prefix of
  -- the next run

  -- Create the first state and insert it into a state stack
  let is = createInitState firstTrans
  let replaySS = [is]

  -- Replay the remaining transitions
  performReplay (s:sts) replaySS sock tidToSock

-- Recursively move down the passed state stack and replay the selected
-- transitions.
--
-- The second state stack is the statestack of the program during the replay.
-- That is, it contains all the executed states so far.
--
-- Returns the statestack of the program after the replay was completed along
-- with the socket map for all the threads
performReplay :: StateStack -> StateStack -> Socket -> TIdToSock -> IO (StateStack, TIdToSock)
performReplay [] newSs _ socks = return (newSs, socks)
performReplay _ [] _ _ = error "Empty replay trace passed"
performReplay (st:sts) (newSt:newSts) sock tidToSock = do
  let selT = selTrans st

  -- This will crash if the selected transition in the replay does not exist in
  -- the next state
  --putStrLn $ "selT:\n" ++ (show selT) ++ "\n"
  let nextT = getTransSetTId (getTransTId selT) (enabledSet newSt)

  -- Since this is a new state, we clear the DoneSet of the state so it does
  -- not contain the doneset ofthe previous state
  ((prevState, nextState), newSocks) 
      <- executeTrans (st {doneSet = Set.empty}) nextT sock tidToSock
  -- Continue building the replay stack
  let newSs = nextState : updateBacktrackSet (prevState : (tail (newSt:newSts)))
  performReplay sts newSs sock newSocks

-- Execute the passed transitions which is assumed to be from the passed state
--
-- This will return (prevState, nextState) where prevState is an updated
-- version of the passed state reflecting the execution of the passed
-- transition.
executeTrans :: State -> Transition -> Socket -> TIdToSock -> IO ((State, State), TIdToSock)

executeTrans st tr sock socks = do
  -- Update the selected transition of the state
  --let prevState = updateDoneSelAfterExec tr (st { doneSet = Set.empty })
  let prevState = updateDoneSelAfterExec tr st
  --putStrLn $ "Executing: " ++ show tr
  -- Socket connected to the thread of the selected transition
  let selSock = socks Map.! (getTransTId tr)
  -- Let the selected thread proceed
  --putStrLn "Sending permit"
  threadPermit selSock
  -- Wait for any thread which should communicate with the socket after
  -- executing this transition. Except if the selected transition is an
  -- assert fail: at this point, we can stop
  when (isAssertFail tr) $ error "Found assertion violation"
  (ts, newSocks) <- getNextTransitions tr sock socks
  when (invalidNextTrans tr ts)
       $ error ("invalid next transitions: executed " ++ (show tr) 
      ++ "\nnext trans: " ++ (show ts))

  -- Create a new state reflecting the execution of selTrans and the current
  -- state of the thread (newTrans)
  return $ ((prevState, (updateStateAfterExec prevState tr ts)), newSocks)

-- Command line options
data Options = Options { 
                       -- Path to the program to execute. If it is empty,
                       -- then the program runs in standalone mode (the user
                       -- is required to execute the program)
                         programPath :: String 
                       -- Arguments to pass to program under test
                       , programArgs :: [String]
                       -- Verbose output
                       , verbose :: Bool
                       , disHelp :: Bool
                       } 

defaultOptions :: Options
defaultOptions = Options { programPath = ""
                         , programArgs = []
                         , verbose = False
                         , disHelp = False
                         }

-- Option parser. Maps an Option to a new Option with its records updated. IO
-- is used to easily print information and exit
options :: [OptDescr (Options -> Options)]
options = [
            Option ['v'] ["verbose"]
            (NoArg (\opt -> opt { verbose = True }))
            "verbose output"
          , 
            Option ['h'] ["help"]
            (NoArg (\opt -> opt { disHelp = True }))
            "display help"
          ]

-- Process command line arguments. 
getCmdOpts :: [String] -> IO Options
getCmdOpts args = do
  case getOpt Permute options args of
    -- For now, any non-options are ignored (item 2 of the tuple)
    (actions, nonOpts , []) -> case (length nonOpts) of
            0 -> return (foldl (flip id) defaultOptions actions)
            _ -> return (foldl (flip id)
                (defaultOptions {
                    programPath = (head nonOpts)
                  , programArgs = tail nonOpts
                    }) actions)
    (_, _, errs) 
        -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "Usage: SysTest [-vh] <prog name>"

exitUsage :: IO ()
exitUsage = do
  putStrLn $ "Usage: SysTest [-vh] <prog name> <prog args>\n"
          ++ "--verbose (-v): enable verbose output\n"
          ++ "--help (-h): display this message\n"
          ++ "If no program name is passed, then standalone mode is used.\n"
          ++ "In standalone, the scheduler will wait for the user to execute."
          ++ "\nthe program under test."
  exitFailure
        

main :: IO ()
main = withSocketsDo $ do 
  args <- getArgs
  opts <- getCmdOpts args

  -- Delete the old socket incase it exists
  cleanupSocket sockPathStr
  -- Create, bind, and start listening on a new socket.
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix sockPathStr)
  listen sock maxThreads

  when (verbose opts) $ putStrLn "Verbose output"
  when (disHelp opts) $ exitUsage

  -- TODO: If we want to fully control the program, it should be executed here
  execProcIfNotStandalone opts
  -- Get the first transition
  (firstTrans, tidToSock) <- threadRecv sock Map.empty

  -- Create the first state and insert it into a state stack
  let is = createInitState firstTrans
  let ss = [is]

  -- The first run is a free run
  stateStack <- performFreeRun ss sock tidToSock
  when (verbose opts) $ putStrLn $ prettyPrintStack stateStack

  putStrLn "====== Run 1 ====="

  -- Calculate the prefix and continue the next runs
  let prefix = getPrefix stateStack
  explore sock prefix opts 2
  --when (null ret) $ error "explore returned non-empty stack"
  putStrLn "Exploration complete"

  -- TODO: Close all sockets in tidToSock map?
  sClose sock
  cleanupSocket sockPathStr
  return ()
