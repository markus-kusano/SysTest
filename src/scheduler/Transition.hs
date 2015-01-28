module Transition (Transition(..), byteStringToTrans, getTransTId, sameTId,
sameTId2, mayEnable, mayDisable, TId, MutId, CondId, isJoin, isThreadCreate, transTIdToString, isAssertFail, TId) where 
import qualified Data.ByteString as B
import Data.Word (Word64)
import Data.Binary.Strict.Get

-- Export newtypes for thread, condition variable, mutex, object, and
-- instruction IDs so they cannot be compared with each other
newtype TId = TId Word64 
              deriving(Show, Eq, Ord, Bounded)
newtype CondId = CondId Word64
              deriving(Show, Eq, Ord, Bounded)
newtype MutId = MutId Word64
              deriving(Show, Eq, Ord, Bounded)
newtype ObjId = ObjId Word64
              deriving(Show, Eq, Ord, Bounded)
newtype InstId = InstId Word64
              deriving(Show, Eq, Ord, Bounded)


-- This type represents parsed data recieved from the program under test. If it
-- is valid, it is converted to a Transition.
data RawTransition = RawTransition { rawTransType :: TransType
                             , rawObjId :: Word64
                             , rawMutId :: Word64
                             , rawCondId :: Word64
                             , rawInstId :: Word64
                             , rawTId :: Word64 }
                             deriving(Show, Eq)

-- Type representing the different possible transitions
data Transition = LoadPre { tId :: TId
                          , objId :: ObjId
                          , instId :: InstId }
                | StorePre { tId :: TId
                           , objId :: ObjId
                           , instId :: InstId}
                | ThreadEnd { tId :: TId }
                | ThreadCreate { tId :: TId 
                               , childTId :: TId }
                | ThreadJoin { tId :: TId
                             , childTId :: TId }
                | MutDestroy { tId :: TId
                             , mutId :: MutId }
                | MutLock { tId :: TId
                          , mutId :: MutId }
                | MutUnlock { tId :: TId
                            , mutId :: MutId }
                | CondDestroy { tId :: TId
                              , condId :: CondId }
                | CondWait { tId :: TId
                           , condId :: CondId 
                           , mutId :: MutId }
                | CondBroadcast { tId :: TId
                                , condId :: CondId }
                | CondSignal { tId :: TId
                             , condId :: CondId }
                | AssertFail { tId :: TId }
                | Invalid
                deriving(Show, Eq, Ord) 

-- Returns the thread ID of the passed transition.
--
-- This is valid for all transition types since every transition type has a
-- thread ID
getTransTId :: Transition -> TId
getTransTId (LoadPre t _ _)      = t
getTransTId (StorePre t _ _)     = t
getTransTId (ThreadEnd t)        = t
getTransTId (ThreadCreate t _ )  = t
getTransTId (ThreadJoin t _)     = t
getTransTId (MutDestroy t _)     = t
getTransTId (MutLock t _)        = t
getTransTId (MutUnlock t _)      = t
getTransTId (CondDestroy t _)    = t
getTransTId (CondWait t _ _)     = t
getTransTId (CondBroadcast t _)  = t
getTransTId (CondSignal t _ )    = t
getTransTId (AssertFail t)       = t
getTransTId (Invalid) = TId 9999

-- Returns true if the two passed transitions have the same TId
sameTId :: Transition -> Transition -> Bool
sameTId t1 t2 = (getTransTId t1) == (getTransTId t2)

sameTId2 :: TId -> Transition -> Bool
sameTId2 tid t2 = tid == (getTransTId t2)

-- Returns true if the passed transition is a join
isJoin :: Transition -> Bool
isJoin (ThreadJoin _ _) = True
isJoin _ = False

-- Return true if the passed transition is a join and if the join is on the
-- passed TId
isJoinAndChild :: TId -> Transition -> Bool
isJoinAndChild t (ThreadJoin _ c) = t == c
isJoinAndChild _ _ = False

isThreadCreate :: Transition -> Bool
isThreadCreate (ThreadCreate _ _ ) = True
isThreadCreate _ = False

-- Return a string form of the ID of the thread executing the passed transition
transTIdToString :: Transition -> String
transTIdToString t = show $ getTransTId t

-- Returns true if the passed transition is an assertion failure. Otherwise,
-- false
isAssertFail :: Transition -> Bool
isAssertFail (AssertFail _) = True
isAssertFail _ = False

-- mayEnable t1 t2:
-- Returns true if t1 may enable t2
--
mayEnable :: Transition -> Transition -> Bool
-- A child thread exiting enables any thread joining on the child
mayEnable (ThreadEnd tid) (ThreadJoin _ cid) = tid == cid
-- A mutex unlock enables a mutex lock to the same mutex
mayEnable (MutUnlock _ m1) (MutLock _ m2) = m1 == m2
-- A cond signal/broadcast enables a cond wait. Note, you also need to check if
-- the lock associated with the condition variable is available
mayEnable (CondSignal _ cid1) (CondWait _ cid2 _) = cid1 == cid2
mayEnable (CondBroadcast _ cid1) (CondWait _ cid2 _) = cid1 == cid2
mayEnable _ _ = False

-- mayDisable t1 t2:
-- Returns true if t1 may disable t2
mayDisable :: Transition -> Transition -> Bool
mayDisable (MutLock _ m1) (MutLock _ m2) = m1 == m2
-- CondWaits to the same mutex disable each other as well. This is because even
-- if they both wakeup they will still race to lock the same mutex
mayDisable (CondWait _ _ m1) (CondWait _ _ m2) = m1 == m2
mayDisable _ _ = False



-- Convert a RawTransition to a Transition.
--
-- This function will call error if any of the fields are malformed
rawTransToTransition :: RawTransition -> Transition
rawTransToTransition rt 
  | ((ty == LoadPre_t)
    && (oid /= (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid /= (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = LoadPre { tId = t, objId = oid, instId = iid }
  | ((ty == StorePre_t)
    && (oid /= (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid /= (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = StorePre { tId = t, objId = oid, instId = iid }
  | ((ty == ThreadEnd_t)
    && (oid == (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = ThreadEnd { tId = t }
  | ((ty == ThreadCreate_t)
    && (oid /= (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = ThreadCreate { tId = t, childTId = childId}
  | ((ty == ThreadJoin_t)
    && (oid /= (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = ThreadJoin { tId = t, childTId = childId }
  | ((ty == MutDestroy_t)
    && (oid == (maxBound :: ObjId))
    && (mid /= (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = MutDestroy { tId = t, mutId = mid }
  | ((ty == MutLock_t)
    && (oid == (maxBound :: ObjId))
    && (mid /= (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = MutLock { tId = t, mutId = mid }
  | ((ty == MutUnlock_t)
    && (oid == (maxBound :: ObjId))
    && (mid /= (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = MutUnlock { tId = t, mutId = mid }
  | ((ty == CondDestroy_t)
    && (oid == (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid /= (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = CondDestroy { tId = t, condId = cid }
  | ((ty == CondWait_t)
    && (oid == (maxBound :: ObjId))
    && (mid /= (maxBound :: MutId))
    && (cid /= (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = CondWait { tId = t, condId = cid, mutId = mid }
  | ((ty == CondBroadcast_t)
    && (oid == (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid /= (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = CondBroadcast { tId = t, condId = cid }
  | ((ty == CondSignal_t)
    && (oid == (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid /= (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = CondSignal { tId = t, condId = cid }
  | ((ty == AssertFail_t)
    && (oid == (maxBound :: ObjId))
    && (mid == (maxBound :: MutId))
    && (cid == (maxBound :: CondId))
    && (iid == (maxBound :: InstId))
    && (t /= (maxBound :: TId)))
    = AssertFail { tId = t }
  | otherwise = error $ "Invalid raw transition: " ++ (show rt)
    where ty = rawTransType rt
          oid = ObjId (rawObjId rt)
          mid = MutId (rawMutId rt)
          cid = CondId (rawCondId rt)
          -- The object slot of the transition is either an object or for
          -- thread join it is a thread ID
          childId = TId (rawObjId rt)
          iid = InstId (rawInstId rt)
          t = TId (rawTId rt)


{- Types of transitions handled -}
data TransType =  LoadPre_t         -- 0
                | StorePre_t        -- 1
                | ThreadEnd_t       -- 2
                | ThreadCreate_t    -- 3
                | ThreadJoin_t      -- 4
                | MutDestroy_t      -- 5
                | MutLock_t         -- 6
                | MutUnlock_t       -- 7
                | CondDestroy_t     -- 8
                | CondWait_t        -- 9
                | CondBroadcast_t   -- 10
                | CondSignal_t      -- 11
                | AssertFail_t      -- 12
                | Invalid_t         -- 999
                deriving(Show, Eq)

-- Convert an integer (passed from the program under test) to a TransType
--
-- This function will call error if the Word64 is not recognized
intToTransType :: Word64 -> TransType
intToTransType 0 = LoadPre_t
intToTransType 1 = StorePre_t
intToTransType 2 = ThreadEnd_t
intToTransType 3 = ThreadCreate_t
intToTransType 4 = ThreadJoin_t
intToTransType 5 = MutDestroy_t
intToTransType 6 = MutLock_t
intToTransType 7 = MutUnlock_t
intToTransType 8 = CondDestroy_t
intToTransType 9 = CondWait_t
intToTransType 10 = CondBroadcast_t
intToTransType 11 = CondSignal_t
intToTransType 12 = AssertFail_t
intToTransType 999 = Invalid_t
intToTransType a = error $ "intToTransType: unhandled Word64: " ++ (show a)

{-- process raw socket data to a transition --}
byteStringToTrans :: B.ByteString -> Transition
byteStringToTrans bstr = 
  case runGet parseTransition bstr of
    (Right t, _) -> rawTransToTransition t
    (Left s, _) -> error $ "unable to parse transition: " ++ s


-- Strictly parse a bytestring as a RawTransition
parseTransition :: Get RawTransition
parseTransition = do 
  tType <- getWord64le
  oId <- getWord64le
  mId <- getWord64le
  cId <- getWord64le
  iId <- getWord64le
  tid <- getWord64le
  return RawTransition { rawTransType = (intToTransType tType)
                    , rawObjId = oId
                    , rawMutId = mId
                    , rawCondId = cId
                    , rawInstId = iId
                    , rawTId = tid }

