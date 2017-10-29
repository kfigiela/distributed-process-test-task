{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Broadcaster where

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process (NodeId, Process, ProcessId,
                                              getSelfPid, match, nsendRemote,
                                              receiveTimeout, say, send)
import           Control.Monad               (forM_, void)
import           Control.Monad.Trans         (liftIO)
import           Data.Time.Clock             (getCurrentTime)
import           Data.Typeable               (Typeable)
import           GHC.Generics
import           System.Random               (StdGen, randoms)

import           Data.Binary.Orphans         (Binary)
import           Message                     (Message (..))


newtype FinishBroadcasting   = FinishBroadcasting   ProcessId deriving (Generic, Typeable, Binary)
newtype BroadcastFinished = BroadcastFinished Int       deriving (Generic, Typeable, Binary)

findIndexM :: Monad m => (a -> m (Maybe b)) -> [a] -> m (Int, Maybe b)
findIndexM = findIndexM' 0 where
    findIndexM' count check [] = return (count, Nothing)
    findIndexM' count check (h:t) = do
        result <- check h
        case result of
            Just value -> return (count, Just value)
            Nothing    -> findIndexM' (count + 1) check t



broadcaster :: [NodeId] -> String -> StdGen -> Process ()
broadcaster peers port g = do
        say ("hello " ++ port)
        self <- getSelfPid
        let values = randoms g
            messages = Message self <$> [0..] <*> values
        (sentMsgs, replyTo) <- flip findIndexM messages $ \msg -> do
            shouldFinish <- receiveTimeout 0 [match $ \(FinishBroadcasting replyTo) -> return $ Just replyTo]

            case shouldFinish of
                Nothing -> do
                    liftIO $ threadDelay 1000
                    -- say $ "known peers: " ++ show (length peers)
                    currentTime <- liftIO getCurrentTime
                    let msg' = msg currentTime
                    forM_ peers $ \peer -> nsendRemote peer "collector" msg'
                    return Nothing
                Just mayReplyTo -> do
                    say "Broadcast stopped"
                    return mayReplyTo
        say $ "sent " ++ show sentMsgs
        forM_ replyTo $ flip send $ BroadcastFinished sentMsgs
