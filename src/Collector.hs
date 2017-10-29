{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Collector where
import           GHC.Generics

import           Control.Distributed.Process (NodeId, Process, ProcessId,
                                              getSelfPid, match, nsendRemote,
                                              receiveWait, register, say, send)
import           Control.Lens
import           Control.Monad               (forM_, forever, unless, void,
                                              when)
import           Control.Monad.Trans         (lift, liftIO)


import           Data.Binary.Orphans         (Binary)
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Time.Clock             (UTCTime, getCurrentTime)
import qualified Data.Time.Clock             as Clock
import           Data.Typeable               (Typeable)

import           Message

whileRightM :: Monad m => Either b a -> (a -> m (Either b a)) -> m b
whileRightM (Left result) action = return result
whileRightM (Right state) action = action state >>= flip whileRightM action

data State = State { _messages     :: Set Message
                   , _lastSequence :: HashMap NodeId Int
                   } deriving (Generic, Typeable, Binary, Show)

makeLenses ''State

initState = State Set.empty HashMap.empty

type Result = (Int, Double)

newtype FinishRequest  = FinishRequest ProcessId deriving (Generic, Typeable, Binary, Show)
newtype FinishResponse = FinishResponse Result   deriving (Generic, Typeable, Binary, Show)

data    RetransmissionRequest  = RetransmissionRequest ProcessId NodeId Int Int deriving (Generic, Typeable, Binary, Show)
newtype RetransmissionResponse = RetransmissionResponse [Message]               deriving (Generic, Typeable, Binary, Show)


computeScore :: State -> Double
computeScore State { _messages = messages } = snd $ Set.foldr' f (1, 0.0) messages where
    f Message { _value = value} (ix, acc) = (ix + 1, acc + ix * value)

computeResult :: State -> Result
computeResult state@State { _messages = messages } = (Set.size messages, computeScore state)


requestRetransmission :: NodeId -> Int -> Int -> Process ()
requestRetransmission node from to = do
    self <- getSelfPid
    say $ "Requestin retransmission: " ++ (show node) ++ " range: " ++ (show from) ++ " - " ++ (show to)
    nsendRemote node "collector" $ RetransmissionRequest self node from to

processMessage :: State -> Message -> Process (Either Result State)
processMessage state message = do
    let lastSeq = state ^. lastSequence . at nodeId
        nodeId = message ^. source
        currentSeq = message ^. sequenceNumber

    case lastSeq of
        Nothing ->
            unless (currentSeq == 0) $ requestRetransmission nodeId 0 currentSeq
        Just lastSeq ->
            unless (currentSeq == lastSeq + 1) $ requestRetransmission nodeId lastSeq currentSeq


    return $ Right $ state & messages                 %~ Set.insert message
                           & lastSequence . at nodeId ?~ currentSeq

processRetransmissionRequest :: State -> RetransmissionRequest -> Process (Either Result State)
processRetransmissionRequest state (RetransmissionRequest replyTo nodeId from to) = do
    let nodeMessages = Set.filter (\el -> el ^. source == nodeId) (state ^. messages)
        foundMessages = Set.takeWhileAntitone (\el -> el ^. sequenceNumber < to) $ Set.dropWhileAntitone (\el -> el ^. sequenceNumber < from) $ nodeMessages
    send replyTo $ RetransmissionResponse $ Set.toList foundMessages
    return $ Right state


processRetransmissionResponse :: State -> RetransmissionResponse -> Process (Either Result State)
processRetransmissionResponse state (RetransmissionResponse newMessages) = do
    say $ "Got some retransmission  " ++ (show $ length newMessages)
    return $ Right $ state & messages %~ Set.union (Set.fromList newMessages)

processFinish :: State -> FinishRequest -> Process (Either Result State)
processFinish state (FinishRequest replyTo) = do
    let result = computeResult state
    -- say $ show result
    send replyTo $ FinishResponse result
    return $ Left result

collector :: Process ()
collector = void $ do
    collectorPid <- getSelfPid
    register "collector" collectorPid

    whileRightM (Right initState) (\state ->
            receiveWait [ match $ processFinish state
                        , match $ processMessage state
                        , match $ processRetransmissionResponse state
                        , match $ processRetransmissionRequest  state
                        ]
        )
