{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Collector where
import           GHC.Generics

import           Control.Distributed.Process (NodeId, Process, ProcessId,
                                              getSelfPid, match, nsendRemote,
                                              receiveWait, register, say, send)
import           Control.Lens
import           Control.Monad               (unless, void)

import           Data.Binary.Orphans         (Binary)
import           Data.Foldable               (maximumBy, minimumBy)
import           Data.Function               (on)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Maybe                  (fromMaybe)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable               (Typeable)

import           Message

whileRightM :: Monad m => Either b a -> (a -> m (Either b a)) -> m b
whileRightM (Left result) action = return result
whileRightM (Right state) action = action state >>= flip whileRightM action

data State = State { _messages             :: !(Set Message)
                   , _lastSequence         :: !(HashMap NodeId Int)
                   , _nodesThatFinished    :: !(Set NodeId)
                   , _precomputedAcc       :: !Double
                   , _precomputedCount     :: !Int
                   , _precomputedThreshold :: !(Maybe UTCTime)
                   , _precomputedMaximums  :: !(HashMap NodeId (Int, UTCTime))
                   , _parentPid            :: ProcessId
                   } deriving (Generic, Typeable, Binary, Show)

makeLenses ''State

initState :: ProcessId -> State
initState = State Set.empty HashMap.empty Set.empty 0.0 0 Nothing HashMap.empty

type Result = (Int, Double)

data FinalizeRequest  = FinalizeRequest  deriving (Generic, Typeable, Binary, Show)
data PrecomputeResultRequest = PrecomputeResultRequest deriving (Generic, Typeable, Binary, Show)
newtype FinalResult = FinalResult Result   deriving (Generic, Typeable, Binary, Show)

data    RetransmissionRequest  = RetransmissionRequest ProcessId NodeId Int Int deriving (Generic, Typeable, Binary, Show)
newtype RetransmissionResponse = RetransmissionResponse [Message]               deriving (Generic, Typeable, Binary, Show)


serviceName :: String
serviceName = "collector"

computeScore :: Set Message -> Double
computeScore messages = snd $ Set.foldl' f (0, 0.0) messages where
    f (ix, acc) Message { _value = value} = (newIx, acc + fromIntegral newIx * value) where
        newIx = ix + 1

computeScore' :: Set Message -> (Int, Double) -> (Int, Double)
computeScore' messages (cix, acc) = Set.foldl f (cix, acc) messages where
    f (ix, acc) Message { _value = value} = (newIx, acc + fromIntegral newIx * value) where
        newIx = ix + 1

computeResult :: State -> Result
computeResult state@State { _messages = messages } = computeScore' messages (state ^. precomputedCount, state ^. precomputedAcc)

precomputeResult :: Maybe Int -> State -> State
precomputeResult forceAt state = state & messages             .~ remainingMessages
                                       & precomputedAcc       .~ newAcc
                                       & precomputedCount     .~ newCount
                                       & precomputedMaximums  .~ newMaximums
                                       & precomputedThreshold ?~ minTimestamp
    where
        (newCount, newAcc) = computeScore' messagesToPrecompute (state ^. precomputedCount, state ^. precomputedAcc)

        oldMessages = state ^. messages

        (messagesToPrecompute, remainingMessages) = case forceAt of
                                                        Just index -> Set.splitAt index oldMessages
                                                        Nothing -> Set.spanAntitone (\msg -> msg ^. timestamp < minTimestamp) oldMessages

        minTimestamp, maxTimestamp :: UTCTime
        (_, minTimestamp) = minimumBy (compare `on` snd) newMaximums
        (_, maxTimestamp) = maximumBy (compare `on` snd) newMaximums

        newMaximums = computeMaximums (state ^. precomputedMaximums) oldMessages

        computeMaximums :: HashMap NodeId (Int, UTCTime) -> Set Message -> HashMap NodeId (Int, UTCTime)
        computeMaximums = Set.foldl' f where
            f :: HashMap NodeId (Int, UTCTime) -> Message -> HashMap NodeId (Int, UTCTime)
            f acc msg = if shouldIncrement
                            then acc & at (msg ^. source) ?~ (msg ^. sequenceNumber, msg ^. timestamp)
                            else acc
                where
                    shouldIncrement = fromMaybe True $ isSequential <$> lastSequential
                    lastSequential = acc ^. at (msg ^. source)
                    isSequential (lastSeq, _) = msg ^. sequenceNumber == lastSeq + 1


requestRetransmission :: NodeId -> Int -> Int -> Process ()
requestRetransmission node from to = do
    self <- getSelfPid
    nsendRemote node serviceName $ RetransmissionRequest self node from to
    say $ "Requesting retransmission from " ++ (show node) ++ " in range from " ++ (show from) ++ " to " ++ (show to)

hasFinished :: State -> Bool
hasFinished state = Set.size (state ^. nodesThatFinished) == HashMap.size (state ^. lastSequence)

processPrecomputeResult :: State -> PrecomputeResultRequest -> Process (Either Result State)
processPrecomputeResult state _ = do
    let newState = precomputeResult Nothing state

    if hasFinished newState
        then do
            say "Got results ahead of time"
            finalize newState
        else return $ Right newState

optimizationThreshold :: Int
optimizationThreshold = 10000 -- messages

optimizationFrequency :: Int
optimizationFrequency = 1000 -- messages

forcedOptimizationRange :: Int
forcedOptimizationRange = 2 * optimizationThreshold -- messages

bufferLimit :: Int
bufferLimit = 4 * optimizationThreshold -- messages

optimizeState :: State -> Process (State)
optimizeState state
    | length (state ^. messages) >= optimizationThreshold && length (state ^. messages) `mod` optimizationFrequency == 0 = do
        let optimizedState = precomputeResult Nothing state
        say $ "Optimized from " ++ show (length $ state ^. messages) ++ " to " ++ show (length $ optimizedState ^. messages)
        return optimizedState
    | length (state ^. messages) >= bufferLimit = do
        let optimizedState = precomputeResult (Just forcedOptimizationRange) state
        say $ "FORCED precomputation from " ++ show (length $ state ^. messages) ++ " to " ++ show (length $ optimizedState ^. messages)
        return optimizedState
    | otherwise = return state


processMessage :: State -> Message -> Process (Either Result State)
processMessage state message = do
    let newState   = state & messages                 %~ Set.insert message
                           & lastSequence . at nodeId ?~ currentSeq
        acceptMessage = maybe True (< (message ^. timestamp)) (state ^. precomputedThreshold)
        currentSeq = message ^. sequenceNumber
        nodeId     = message ^. source
        requestRetransmissionIfNeeded = unless (currentSeq - 1 == lastSeq) $ requestRetransmission nodeId lastSeq currentSeq
            where lastSeq    = fromMaybe 0 $ state ^. lastSequence . at nodeId

    if acceptMessage
    then do
        requestRetransmissionIfNeeded

        optimizedState <- optimizeState newState
        return $ Right optimizedState
    else do
        say "Rejected message: already precomputed"
        return $ Right state

processRetransmissionRequest :: State -> RetransmissionRequest -> Process (Either Result State)
processRetransmissionRequest state (RetransmissionRequest replyTo nodeId from to) = do
    let nodeMessages = Set.filter (\el -> el ^. source == nodeId) (state ^. messages)
        foundMessages = Set.takeWhileAntitone (\el -> el ^. sequenceNumber < to) $ Set.dropWhileAntitone (\el -> el ^. sequenceNumber < from) nodeMessages
    send replyTo $ RetransmissionResponse $ Set.toAscList foundMessages
    return $ Right state


processRetransmissionResponse :: State -> RetransmissionResponse -> Process (Either Result State)
processRetransmissionResponse state (RetransmissionResponse newMessages) = do
    say $ "Got some retransmission  " ++ (show $ length newMessages)
    return $ Right $ state & messages %~ Set.union (Set.fromList newMessages)

processNoMoreMessages :: State -> NoMoreMessages -> Process (Either Result State)
processNoMoreMessages state (NoMoreMessages nodeId) = return $ Right $ state & nodesThatFinished %~ Set.insert nodeId

finalize :: State -> Process (Either Result State)
finalize state = do
    let result = computeResult state
    send (state ^. parentPid) $ FinalResult result
    return $ Left result

processFinalize :: State -> FinalizeRequest -> Process (Either Result State)
processFinalize state FinalizeRequest = finalize state

collector :: ProcessId -> Process ()
collector parent = void $ do
    collectorPid <- getSelfPid
    register serviceName collectorPid

    whileRightM (Right $ initState parent) (\state ->
            receiveWait [ match $ processFinalize state
                        , match $ processPrecomputeResult state
                        , match $ processMessage state
                        , match $ processNoMoreMessages state
                        , match $ processRetransmissionResponse state
                        , match $ processRetransmissionRequest  state
                        ]
        )
