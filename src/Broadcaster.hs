{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Broadcaster where

import           GHC.Generics

import           Control.Concurrent          (threadDelay)
import           Control.Distributed.Process (NodeId, Process, ProcessId,
                                              expect, getSelfNode, getSelfPid,
                                              match, nsendRemote,
                                              receiveTimeout, register, say,
                                              send)
import           Control.Monad               (forM_)
import           Control.Monad.Trans         (liftIO)
import           Data.Time.Clock             (UTCTime, getCurrentTime)
import           Data.Typeable               (Typeable)
import           System.Random               (StdGen, randoms)

import           Data.Binary.Orphans         (Binary)

import           Broadcaster.API
import qualified Collector                   (serviceName)
import           Message                     (Message (..), NoMoreMessages (..))
import           PeerManager                 (getPeers)
import qualified Services

countWhileJustM_ :: Monad m => (a -> b -> m (Maybe b)) -> b -> [a] -> m Int
countWhileJustM_ = countWhileJustM' 0 where
    countWhileJustM' count check _ [] = return count
    countWhileJustM' count check state (h:t) = do
        result <- check h state
        case result of
            Just state' -> countWhileJustM' (count + 1) check state' t
            Nothing     -> return count


broadcasterLoop :: (UTCTime -> Message) -> [NodeId] -> Process (Maybe [NodeId])
broadcasterLoop msg peers = do
    currentTime <- liftIO getCurrentTime
    liftIO $ threadDelay 1 -- without this I run out of file descriptors when running locally larger number of nodes, WTF?
    let msg' = msg currentTime
    forM_ peers $ \peer -> nsendRemote peer Collector.serviceName msg'

    gotMessage <- receiveTimeout 0 [ match $ \FinishBroadcasting     -> return (True,  peers)
                                   , match $ \(NewPeerList newPeers) -> return (False, newPeers)]

    case gotMessage of
        Nothing -> return $ Just peers
        Just (True, _) -> do
            say "Broadcast finished"
            localNode  <- getSelfNode
            forM_ peers $ \peer -> nsendRemote peer Collector.serviceName $ NoMoreMessages localNode
            return Nothing
        Just (False, newPeers) -> do
            say $ "Broadcaster got new peers"
            return $ Just newPeers


broadcaster :: StdGen -> Process ()
broadcaster g = do
    self <- getSelfPid
    register Services.broadcaster self

    localNode  <- getSelfNode
    let values = randoms g
        messages = zipWith (Message localNode) [1..] values

    peers <- getPeers

    sentMsgs <- countWhileJustM_ broadcasterLoop peers messages

    say $ "Sent " ++ show sentMsgs ++ " messages in total"
