{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Broadcaster where

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process (NodeId, Process, getSelfPid,
                                              match, nsendRemote,
                                              receiveTimeout, say)
import           Control.Monad               (forM_, void)
import           Control.Monad.Trans         (liftIO)
import           Data.Time.Clock             (getCurrentTime)
import           Data.Typeable               (Typeable)
import           GHC.Generics
import           System.Random               (StdGen, randoms)

import           Data.Binary.Orphans         (Binary)
import           Message                     (Message (..))


data FinishBroadcasting = FinishBroadcasting deriving (Generic, Typeable, Binary)

findIndexM :: Monad m => (a -> m Bool) -> [a] -> m Int
findIndexM = findIndexM' 0 where
    findIndexM' count check [] = return count
    findIndexM' count check (h:t) = do
        result <- check h
        if result
        then return count
        else findIndexM' (count + 1) check t



broadcaster :: [NodeId] -> String -> StdGen -> Process ()
broadcaster peers port g = do
        say ("hello " ++ port)
        self <- getSelfPid
        let values = randoms g
            messages = Message self <$> [0..] <*> values
        sentMsgs <- flip findIndexM messages $ \msg -> do
            shouldFinish <- receiveTimeout 0 [match $ \FinishBroadcasting -> return True]

            case shouldFinish of
                Nothing -> do
                    liftIO $ threadDelay 1000
                    -- say $ "known peers: " ++ show (length peers)
                    currentTime <- liftIO getCurrentTime
                    let msg' = msg currentTime
                    forM_ peers $ \peer -> nsendRemote peer "collector" msg'
                    return False
                Just _ -> do
                    say "Broadcast stopped"
                    return True
        say $ "sent " ++ show sentMsgs
