{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Broadcaster where

import           GHC.Generics

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process (Process, ProcessId, getSelfNode,
                                              match, nsendRemote,
                                              receiveTimeout, say, send)
import           Control.Monad               (forM_)
import           Control.Monad.Trans         (liftIO)
import           Data.Time.Clock             (getCurrentTime)
import           Data.Typeable               (Typeable)
import           System.Random               (StdGen, randoms)

import           Data.Binary.Orphans         (Binary)

import qualified Collector                   (serviceName)
import           Message                     (Message (..), NoMoreMessages (..))
import           PeerManager                 (getPeers)


data    FinishBroadcasting = FinishBroadcasting             deriving (Generic, Typeable, Binary)
newtype BroadcastFinished  = BroadcastFinished    Int       deriving (Generic, Typeable, Binary)

findIndexM :: Monad m => (a -> m Bool) -> [a] -> m (Int, Bool)
findIndexM = findIndexM' 0 where
    findIndexM' count check [] = return (count, False)
    findIndexM' count check (h:t) = do
        result <- check h
        if result
            then return (count, True)
            else findIndexM' (count + 1) check t


broadcaster :: StdGen -> Process ()
broadcaster g = do
        self  <- getSelfNode
        let values = randoms g
            messages = zipWith (Message self) [0..] values
        (sentMsgs, replyTo) <- flip findIndexM messages $ \msg -> do
            shouldFinish <- receiveTimeout 0 [match $ \FinishBroadcasting -> return  True]

            case shouldFinish of
                Nothing -> do
                    -- liftIO $ threadDelay 1000
                    peers <- getPeers -- TODO: move this to local state and retreive by messages
                    currentTime <- liftIO getCurrentTime
                    let msg' = msg currentTime
                    forM_ peers $ mapM_ $ \peer -> nsendRemote peer Collector.serviceName msg'
                    return False
                Just _ -> do
                    say "Broadcast stopped"
                    return True
        say $ "sent " ++ show sentMsgs
        peers <- getPeers
        forM_ peers $ mapM_ $ \peer -> nsendRemote peer Collector.serviceName $ NoMoreMessages self
