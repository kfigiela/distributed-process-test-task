{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module PeerManager (peerManager, getPeers) where

import           Control.Concurrent                                 (threadDelay)

import           Control.Distributed.Process                        (NodeId,
                                                                     Process,
                                                                     ProcessId,
                                                                     expect,
                                                                     expectTimeout,
                                                                     getSelfNode,
                                                                     getSelfPid,
                                                                     match,
                                                                     nsendRemote,
                                                                     receiveWait,
                                                                     register,
                                                                     say, send,
                                                                     spawnLocal,
                                                                     whereis)
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend,
                                                                     findPeers)
import           Control.Lens
import           Control.Monad                                      (forM_,
                                                                     forever,
                                                                     mapM,
                                                                     unless,
                                                                     void, when)
import           Control.Monad.Trans                                (liftIO)
import           Data.Binary.Orphans                                (Binary)
import           Data.Set                                           (Set)
import qualified Data.Set                                           as Set
import           Data.Typeable                                      (Typeable)
import           GHC.Generics

import qualified Broadcaster.API                                    as Broadcaster
import qualified Services

data State = State { _peers :: Set NodeId, _connectedPeers :: Set NodeId } deriving (Generic, Typeable, Binary, Show)
makeLenses ''State

newtype GetState  = GetState ProcessId deriving (Generic, Typeable, Binary)
newtype GetStateReply = GetStateReply State deriving (Generic, Typeable, Binary)
data DiscoveredPeers = DiscoveredPeers NodeId [NodeId]  deriving (Generic, Typeable, Binary)

multicastManager :: ProcessId -> Backend -> Process ()
multicastManager parent backend = do
    me <- getSelfNode
    forever $ do
        peers <- liftIO $ findPeers backend 1000000
        say $ "Multicast: got peers " ++ show (length peers)
        send parent $ DiscoveredPeers me peers


loop :: State -> Process State
loop state = do
    newState <- receiveWait [ match $ getStateHandler        state
                            , match $ discoveredPeersHandler state
                            ]
    loop newState

peerManager :: [NodeId] -> Bool -> Backend -> Process ()
peerManager knownPeers useMulticast backend = do
    self <- getSelfPid
    localNode <- getSelfNode
    register Services.peerManager self


    when useMulticast $ void $ spawnLocal $ multicastManager self backend
    spawnLocal $ broadcastPeersListProcess self

    let allPeers = localNode:knownPeers

    void $ loop $ State (Set.fromList allPeers) (Set.fromList [localNode])

getStateHandler :: State -> GetState -> Process State
getStateHandler state (GetState replyTo) = do
    send replyTo $ GetStateReply state
    return state

broadcastPeersInterval :: Int
broadcastPeersInterval = 100000

broadcastPeersListProcess :: ProcessId -> Process ()
broadcastPeersListProcess parent = do
    localNode <- getSelfNode
    self <- getSelfPid
    forever $ do
        send parent $ GetState self
        mayState <- expectTimeout broadcastPeersInterval
        forM_ mayState $ \(GetStateReply state) -> do
            let disconnectedPeers = Set.difference (state ^. peers) (state ^. connectedPeers)
            forM_ disconnectedPeers $ \peer -> nsendRemote peer Services.peerManager $ DiscoveredPeers localNode (Set.toAscList $ state ^. peers)
            liftIO $ threadDelay broadcastPeersInterval

discoveredPeersHandler :: State -> DiscoveredPeers -> Process State
discoveredPeersHandler state (DiscoveredPeers source newPeers) = do
    localNode <- getSelfNode

    if source == localNode
    then return state
    else do
        let knownPeers = state ^. peers
            newKnownPeers = Set.union knownPeers $ Set.fromList newPeers

        when (knownPeers /= newKnownPeers) $ do
            say $ "Got new peers: " ++ (show $ Set.size newKnownPeers) ++ " " ++ (show newKnownPeers)
            let newPeersList = Set.toAscList newKnownPeers
            forM_ newKnownPeers $ \peer -> nsendRemote peer Services.peerManager $ DiscoveredPeers localNode $ newPeersList

            sendPeerListToBroadcaster newPeersList

        return $ state & peers          .~ newKnownPeers
                    & connectedPeers %~ Set.insert source

sendPeerListToBroadcaster :: [NodeId] -> Process ()
sendPeerListToBroadcaster nodes = do
    mayBroadcasterPid <- whereis Services.broadcaster
    forM_ mayBroadcasterPid $ \broadcasterPid -> send broadcasterPid $ Broadcaster.NewPeerList nodes

getPeers :: Process [NodeId]
getPeers = whereis Services.peerManager >>= query
    where
        query Nothing = do
            liftIO $ threadDelay 10000
            getPeers
        query (Just processManagerPid) = do
            self <- getSelfPid
            send processManagerPid $ GetState self
            GetStateReply state <- expect
            return $ Set.toAscList $ state ^. peers
