--
-- Author: Markus Kusano
--
-- Implementations and manipulations for States
-- States are simply sets of transitions
module State(State(..), TId, TIdToSock, TransSet, MutId, LockSet, ClockVal,
VectorClock, updateStateAfterExec, updateEnDisSetAfterExec,
updateLockSetAfterExec, updateVecClkAfterExec, createInitState, prettyPrintState, isTIdMember, getTransSetTId, updateDoneSelAfterExec, vcLessThan, disjointLockset, vcHappensBefore, happensBefore, tIdSetInsTrans) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.List
import Network.Socket (Socket)

import Transition (Transition(..), getTransTId, sameTId2, TId, MutId, CondId, isThreadCreate, transTIdToString)

-- Map from tid to socket. The socket has had accept called on it so it is
-- ready for communication
type TIdToSock = Map.Map TId Socket

-- A transition set is a set of transitions
type TransSet = Set.Set Transition

type TIdSet = Set.Set TId

-- A LockSet is a set of Mutex IDs. Mutex IDs are Word64s
type LockSet = Set.Set MutId

-- A vector clock is a map from a thread ID to a clock value. Each thread at
-- each state has their own VectorClock
newtype ClockVal = ClockVal Int
  deriving(Show, Eq, Ord)
type VectorClock = Map.Map TId ClockVal

-- VectorClockMap holds the state of each thread's vector clock in a state.
type VectorClockState = Map.Map TId VectorClock

-- A state is a conglomoration of many other data structures
data State = State { enabledSet :: TransSet
                   , disabledSet :: TransSet
                   , backtrackSet :: TIdSet
                   , doneSet :: TIdSet
                   , lockSet :: LockSet
                   , vecClockState :: VectorClockState 
                   , selTrans :: Transition }
                   deriving(Show)

-- Return a list of the threads in the passed TransSet
transSetTIdToString :: TransSet -> [String]
transSetTIdToString ts = Set.toList $ Set.map transTIdToString ts

tIdSetToString :: TIdSet -> [String]
tIdSetToString ts = Set.toList $ Set.map show ts

-- Insert the TId of the passed transition to the passed TIdSet
tIdSetInsTrans :: Transition -> TIdSet -> TIdSet
tIdSetInsTrans t ts = Set.insert (getTransTId t) ts


-- Return a nice (and somewhat compact) string version of the state
prettyPrintState :: State -> String
prettyPrintState s = "Enabled Set: {" ++ enSetStr ++ "}\n"
             ++ "Disabled Set: {" ++ disSetStr ++ "}\n"
             ++ "Backtrack Set: {" ++ backSetStr ++ "}\n"
             ++ "Done Set: {" ++ doneSetStr ++ "}\n"
             ++ "Selected: " ++ selThdStr ++ "\n"
    --where enSetStr = concat . intersperse ", " . transSetTIdToString $ enabledSet s
    where enSetStr = show $ enabledSet s
          disSetStr = concat . intersperse ", " . transSetTIdToString $ disabledSet s
          backSetStr = concat . intersperse ", " . tIdSetToString $ backtrackSet s
          doneSetStr = concat . intersperse ", " . tIdSetToString $ doneSet s
          selThdStr = show . selTrans $ s

-- Returns a new state based off the passed state assuming that the passed
-- transition was executed from the passed state.
--
-- The second list of transitions passed is the transition which the next
-- threads are about to execute. This is usually only one transition except in
-- the case of a ThreadCreate having just been executed (in this case, it is of
-- size 2).
--
-- This will update the:
-- 1. Done Set
-- 2. Vector clock
-- 3. Lock set (if the executed transition is a lock/unlock operation)
-- 4. Disabled set (if executing this transition will disable any other
-- transitions)
updateStateAfterExec :: State -> Transition -> [Transition] -> State
updateStateAfterExec s selT nextTs
-- TODO: How should/where should the backtrack set be updated?
    = s3
              where
                    oldEnSet = enabledSet s
                    -- Calculate the updated lockSet of the state
                    s1 = updateLockSetAfterExec s selT
                    -- Calculate the updated enabled and disabled sets for the
                    -- state
                    s2 = updateEnDisSetAfterExec selT nextTs s1
                    -- Calculate which transitions were just enabled. These are
                    -- the transitions which were enabled by selT. As a result,
                    -- we need to update their vector clocks to reflect the
                    -- causality
                    --newlyEnabled = (enabledSet s2) Set.\\ oldEnSet
                    newlyEnabled = enabledSet s2
                    -- Update the vectorclock 
                    s3 = updateVecClkAfterExec s2 selT newlyEnabled 

-- Add the passed transition to the done set of the passed state.
updateDoneSelAfterExec :: Transition -> State -> State
--updateDoneSelAfterExec t s = s { doneSet = Set.insert t (doneSet s), selTrans = t }
updateDoneSelAfterExec t s = s { doneSet = tIdSetInsTrans t (doneSet s), selTrans = t }
  --where oldDoneSet = doneSet s

-- Return the transition in the passed set with the passed TId.
-- If no such transition exists, this will crash.
-- If two transitions with the same TId exist, this will crash
getTransSetTId :: TId -> TransSet -> Transition
getTransSetTId t ts = 
  case found of
    (tr:[]) -> tr
    [] -> error "TId not found in transSet"
    (_:_) -> error "Multiple transition w/ same TId"
  where found = Set.toList . Set.filter (sameTId2 t) $ ts

-- Returns true if the passed transition is disabled given the passed lockset
-- set. Otherwise, returns false
lockSetDis :: LockSet -> Transition -> Bool
lockSetDis ls (MutLock _ m) = Set.member m ls
lockSetDis ls (CondWait _ _ m) = Set.member m ls
lockSetDis _ _ = False

-- Returns true if the passed transition is enabled given the passed lockset
-- set
lockSetEn :: LockSet -> Transition -> Bool
lockSetEn ls t = not $ lockSetDis ls t

-- Update the passed enabled set and disabled set based on the execution of the
-- passed transition (the first transition passed).
--
-- This function assumes that the lockset of the passed state is up-to-date
--
-- The second transition passed is the next transition executed by the same
-- thread who executed the first transition. We also need to determine if this
-- thread is enabled or disabled right now.
--
-- The passed state is assumed to be the state where selT was executed
--
-- A transition may disable another transition if:
--    1. The selected transition locked a mutex used by another transition
--
-- A transition may enabled another transition if:
--    1. ThreadEnd of TID will enabled ThreadJoin on child thread with TID
--    2. Selected transition is an unlock of mutex used by another transition
--
-- TODO: How should cond signal and broadcast be handled? The POSIX spec says
-- they may be spuriously awoken: right CondSignal and CondBroadcast are
-- treated in the same manner: after they occurr, all SignalWaits on the same
-- condition variable are converted into MutexLock calls. I believe this allows
-- the most permissive behavior.
--
-- If a CondSignal only awoke one CondWait, then we would have to implement
-- some kind of backtracking to check which CondWait was awoken in future runs. 
updateEnDisSetAfterExec :: Transition -> [Transition] -> State -> State
updateEnDisSetAfterExec selT nextTs st = 
  moveEnDis . updateCondSignal selT . addManyEnabled nextTs . removeFromEnabled selT $ st

-- If the passed Transition is a CondSignal or CondBroadcast, convert any
-- CondWait to the same condition variable to a MutexLock. This simulates the
-- thread on the condition variable being awoken
updateCondSignal :: Transition -> State -> State
updateCondSignal (CondSignal _ cid) s = switchCondToMutLock cid s
updateCondSignal (CondBroadcast _ cid) s = switchCondToMutLock cid s
updateCondSignal _ s = s

-- Convert all occurrances of CondWait on the passed condition variable into
-- MutexLock.
--
-- This searches through both the enabled and disabled set of the state. No
-- transitions are moved from the enabled or disabled set (i.e., they are
-- switched to MutexLock in place)
switchCondToMutLock :: CondId -> State -> State
switchCondToMutLock c s = 
  s { enabledSet = Set.map (condWaitToMutLock c) oldEnSet
    , disabledSet = Set.map (condWaitToMutLock c) oldDisSet}
      where oldEnSet = enabledSet s
            oldDisSet = disabledSet s

-- If the passed transition is a CondWait to the passed condition variable
-- switch it to a MutLock transition to the same mutex as used in the CondWait
condWaitToMutLock :: CondId -> Transition -> Transition
condWaitToMutLock c1 (CondWait t c2 m) = if c1 == c2
                                            then MutLock t m
                                            else (CondWait t c2 m)
condWaitToMutLock _ t = t
                

-- Move any transition in the enabled set which should be disabled to the
-- disabled set (and vice versa).
--
-- This should be called after the enabled, disabled, and locksets of the
-- passed state have been updated.
--
-- CondWait operations (since they are always considered as disabled by
-- isDisabled) are not affected by this function. See updateCondSignal
moveEnDis :: State -> State
moveEnDis s = s { enabledSet = newEnSet
                , disabledSet = newDisSet }
  where oldDisSet = disabledSet s
        oldEnSet = enabledSet s
        -- States which were enabled but are now disabled
        newlyDisabled = Set.filter (isDisabled s) oldEnSet
        -- States which were disabled but are now enabled
        newlyEnabled = Set.filter (not . (isDisabled s)) oldDisSet
        -- The new enabled set is everything that was just enabled along with
        -- everything that was enabled and was not just disabled
        newEnSet = newlyEnabled `Set.union` (oldEnSet Set.\\ newlyDisabled)
        -- The new disabled set is everything that was just disabled along with
        -- everything that was disabled and is not just been enabled
        newDisSet = newlyDisabled `Set.union` (oldDisSet Set.\\ newlyEnabled)

-- Remove the passed transition from the enabled set of the passed state
removeFromEnabled :: Transition -> State -> State
--removeFromEnabled t st = st { enabledSet = Set.delete t oldEn }
removeFromEnabled t st = st { enabledSet = rmTId }
  where oldEn = enabledSet st
        rmTId = Set.delete (getTransSetTId (getTransTId t) oldEn) oldEn

-- Assuming the passed transition was executed, update the disabled set of the
-- state.
--
-- This will also search in the enabled set of the state for any transitions
-- which should be disabled.
--updateDisabledSet :: Transition -> State -> State
--updateDisabledSet t s = 
  -- Transitions which were enabled which should be disabled
  --where newlyDisabled = Set.filter (mayDisable t) (enabledSet st)

-- Returns true if the passed transition is disabled in the passed state
--
-- Note: This does not account for signal required to wakeup a cond_wait.
-- cond_wait is always allowed to wakeup whenever the mutex is availible.
--
isDisabled :: State -> Transition -> Bool
-- Loads and stores can never be disabed
isDisabled _ (LoadPre _ _ _) = False
isDisabled _ (StorePre _ _ _)= False
-- ThreadEnd can never be disabled
isDisabled _ (ThreadEnd _) = False
-- MutexDestroy is never disabled
isDisabled _ (MutDestroy _ _) = False
isDisabled _ (CondDestroy _ _) = False
isDisabled s (MutLock  _ m) = Set.member m (lockSet s)
-- ThreadJoin is disabled if the child thread is still running
isDisabled s (ThreadJoin _ c) = isThdRunning c s
-- CondWait is moved from the enabled to disabled set when it is converted from
-- a CondWait to a MutLock. See updateCondSignal
isDisabled _ (CondWait _ _ _) = True
isDisabled _ (CondBroadcast _ _) = False
isDisabled _ (CondSignal _ _) = False
isDisabled _ (AssertFail _) = False
isDisabled _ (ThreadCreate _ _) = False
isDisabled _ (MutUnlock _ _) = False
isDisabled _ (Invalid) = error "isDisabled called on Invalid"


-- Returns true if the passed TId is still running.
--
-- A thread is still running if it is either enabled or disabled
isThdRunning :: TId -> State -> Bool
isThdRunning t s = (isTIdMember t enSet)
                || (isTIdMember t disSet)
                  where enSet = enabledSet s
                        disSet = disabledSet s

-- Returns true if there is a thread with the passed TId in the passed set
isTIdMember :: TId -> TransSet -> Bool
-- If after the filter the set is not null then the thread is still running (so
-- we should return true)
isTIdMember t ts = not . Set.null . Set.filter (sameTId2 t) $ ts

-- Add the passed list of transitions to the enabled set of the passed state
addManyEnabled :: [Transition] -> State -> State
addManyEnabled (t:ts) s = addManyEnabled ts (addToEnabled t s)
addManyEnabled [] s = s

-- Add passed transition to the enabled set of the passed set
addToEnabled :: Transition -> State -> State
addToEnabled t s = s { enabledSet = Set.insert t oldEn }
  where oldEn = enabledSet s

-- Returns true if the passed transition is enabled in the passed state.
-- Otherwise, returns false
--
-- A transition may be disabled in a state if:
-- 1. It is a transition accessing a lock which is held
-- 2. It is a join on a thread which is still running
-- 3. It is a cond wait or cond signal (these are always disabled until a cond
-- broadcast/signal is seen)
isEnabled :: State -> Transition -> Bool
isEnabled st (MutLock _ m) = Set.member m (lockSet st) 
isEnabled st (CondWait _ _ m) = Set.member m (lockSet st) 
isEnabled _ _ = False

-- Update the passed lockset based on the execution of the passed transition
--
-- The following transitions can update the lockset:
-- 1. CondWait: Obtains a lock
-- 2. MutLock: obtains a lock
-- 3. MutUnlock: releases  a lock
--
updateLockSetAfterExec :: State -> Transition -> State
updateLockSetAfterExec s (CondWait _ _ m) = s { lockSet = m `Set.insert` ls }
  where ls = lockSet s
updateLockSetAfterExec s (MutLock _ m) = s { lockSet = m `Set.insert` ls }
  where ls = lockSet s
updateLockSetAfterExec s (MutUnlock _ m) = s { lockSet = m `Set.delete` ls }
  where ls = lockSet s
updateLockSetAfterExec s _ = s

-- updateVecClkAfterExec t s ts 
-- Given the set of transitions (ts) which were recently enabled after the exec
-- of t from s update the VectorClockState of s.
--
-- This will also update the vector clock set of s if the passed transition was
-- a thread create: the vector clock of the child thread will be updated to
-- reflect the causality
updateVecClkAfterExec :: State -> Transition -> TransSet -> State
-- Only causalaity event currently tracked is ThreadCreate
updateVecClkAfterExec  s (ThreadCreate p c) ts = newS
        -- Always increment the vector clock of the transiting thread
  where t = (ThreadCreate p c)
        s1 = incVcSelf t s
        -- Setup the VC of the child if this is a thread create
        s2 = updateThdCreateVC s1 t
        -- Update the causality
        newS = updateVCSet s2 t (Set.toList ts)
updateVecClkAfterExec s t _ = incVcSelf t s

-- Updates the vector clock of the child thread in the passed state. The passed
-- transition must be a ThreadCreate
updateThdCreateVC :: State -> Transition -> State
updateThdCreateVC s (ThreadCreate p c) = updateVCCausality s parentVC (c, childVC)
  where vcstate = vecClockState s
        parentVC = Map.lookup p vcstate
        childVC = Map.lookup c vcstate
updateThdCreateVC _ t = 
  error $ "updateThdCreateVC called with non thread create: " ++ (show t)

-- Updates the VC of every thread in the passed list of transitions assuming
-- that the passed Transition had a causal effect
updateVCSet :: State -> Transition -> [Transition] -> State
updateVCSet s ct (t:ts) = updateVCSet newS ct ts
  where vcstate = vecClockState s
        causerVC = Map.lookup (getTransTId ct) vcstate
        causedTId = getTransTId t
        causedVC = Map.lookup (causedTId) vcstate
        newS = updateVCCausality s causerVC (causedTId, causedVC)
updateVCSet s _ [] = s

-- updateVCCausality v1 v2
--
-- Update v2 assuming that v1 caused v2.
--
-- Returns the passed state with a modified VectorClockState
updateVCCausality :: State -> Maybe VectorClock -> (TId, Maybe VectorClock) -> State
-- What to do when the selected transition has no vector clock?
updateVCCausality _ Nothing _ = 
  error "updateVCCausality: Selected transition has no vector clock"
-- When the caused thread has no VC then we copy the causer VC and set the
-- clock of the caused to 0
updateVCCausality s (Just v1) (t, Nothing) =  s { vecClockState = Map.insert t (Map.insert t (ClockVal 0) v1) vcstate }
  where vcstate = vecClockState s
-- If both VCs are present, the resulting VC is the max of the two. If an item
-- is not present, then it is assumed to be zero
updateVCCausality s (Just v1) (t, Just v2) = s { vecClockState = Map.insert t (vcMax v1 v2) vcstate }
  where vcstate = vecClockState s

-- Return the state with the VC of the passed transition incremented
--
-- This is the action which every thread takes on a local or non-local
-- transition: they increment their own clock in the VC
incVcSelf :: Transition -> State -> State
incVcSelf t s = case Map.lookup (getTransTId t) (vecClockState s) of
    -- In this case, we likely can just insert a VC of all zeros for the
    -- process
    Nothing -> error "incVcSelf: selected transition has no VC"
    Just vc -> updateVectorClockTId s tid $ incClockVal tid (Map.lookup tid vc) vc
      where tid = getTransTId t

-- Return the state with its vectorClockState as the passed vectorClockState
-- updateVectorClockState :: State -> VectorClockState -> State
--updateVectorClockState s vcs = s { vecClockState = vcs }

-- Return the state with its the vector clock of the passed TId set to the
-- passed vectorclock
updateVectorClockTId :: State -> TId -> VectorClock -> State
updateVectorClockTId s t vc = s { vecClockState = Map.insert t vc vcstate }
  where vcstate = vecClockState s


-- Increment the passed clock value for the passed thread and insert it into
-- the passed VectorClock.
--
-- When Nothing is passed, set the VC to zero
incClockVal :: TId -> Maybe ClockVal -> VectorClock -> VectorClock
incClockVal t Nothing vc = Map.insert t (ClockVal 0) vc
incClockVal t (Just (ClockVal i)) vc = Map.insert t (ClockVal (i + 1)) vc

-- Given the two vector clocks returns a vectorclock which is the max of the
-- two.
--
-- This is simply the union of the two maps using the max function if there is
-- a collision
vcMax :: VectorClock -> VectorClock -> VectorClock
vcMax v1 v2 = Map.unionWith max v1 v2


-- Create a state with only the passed transition in the enabled set. Updates
-- the vector clock of the thread of the passed transition to be zero
createInitState :: Transition -> State
createInitState t = State { enabledSet = Set.insert t Set.empty
                          , disabledSet = Set.empty
                          , backtrackSet = Set.empty
                          , doneSet = Set.empty
                          , lockSet = Set.empty
                          , vecClockState = createInitVCS t 
                          , selTrans = t }


-- Creates a vectorClockState with the thread ID of the passed transition set
-- to zero
createInitVCS :: Transition -> VectorClockState
createInitVCS t = Map.insert tid (Map.insert tid (ClockVal 0) Map.empty) Map.empty
  where tid = getTransTId t

-- Returns true if s1 happens before the passed vectorclock
-- This is calculated by seeing if there exists any vector clock in s1 which
-- happens before the passed vector clock
happensBefore :: State -> VectorClock -> Bool
-- If the result is not empty then atleast one VC happens before the other.
--
-- We flip the arguments of vcHappensBefore since it takes parameters vc1 and
-- vc2 and returns true if vc1 happens before vc2. We are passing it vc2 (we
-- want to see if any vector clock in s1VCState happens before vc)
happensBefore s1 vc = not . Map.null . Map.map ((flip vcHappensBefore) vc) $ s1VCState
  where s1VCState = vecClockState s1


-- Returns true if vc1 happens before vc2. Otherwise, returns false.
vcHappensBefore :: VectorClock -> VectorClock -> Bool
-- If the list is empty, that means no clock in VC1 is less than VC2 so vc1
-- does not happen before vc2. We take the negation
vcHappensBefore vc1 vc2 = not . Map.null . Map.mapWithKey (vcLessThan vc1) $ vc2

-- Returns true if the TId found in the passed vector clock is less than or
-- equal to the passed (TId, ClockVal).
--
-- If the passed VectorClock does not contain the passed TId then it is assumed
-- to be zero
vcLessThan :: VectorClock -> TId -> ClockVal -> Bool
vcLessThan vc tid cv = 
  case Map.lookup tid vc of
    -- Zero is the lowest possible clock value so it is always less than
    Nothing -> True
    Just cv2 -> cv2 < cv

-- Returns true if the two passed states have disjoint locksets. Otherwise,
-- returned false
disjointLockset :: State -> State -> Bool
disjointLockset s1 s2 = Set.null $ Set.intersection ls1 ls2
  where ls1 = lockSet s1
        ls2 = lockSet s2
