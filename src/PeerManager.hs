{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module PeerManager (peerManager, getPeers) where

import           Control.Concurrent                                 (threadDelay)

import           Control.Distributed.Process                        (NodeId,
                                                                     Process,
                                                                     ProcessId,
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
import           Control.Monad                                      (forM,
                                                                     forM_,
                                                                     forever,
                                                                     void, when)
import           Control.Monad.Trans                                (liftIO)
import           Data.Binary.Orphans                                (Binary)
import           Data.Set                                           (Set)
import qualified Data.Set                                           as Set
import           Data.Typeable                                      (Typeable)
import           GHC.Generics

newtype GetPeers  = GetPeers ProcessId deriving (Generic, Typeable, Binary)
newtype GetPeersReply = GetPeersReply [NodeId] deriving (Generic, Typeable, Binary)
newtype DiscoveredPeers = DiscoveredPeers [NodeId]  deriving (Generic, Typeable, Binary)
data    HelloRemote = HelloRemote NodeId [NodeId]  deriving (Generic, Typeable, Binary)

newtype State = State { _peers :: Set NodeId }

makeLenses ''State

initState :: State
initState = State Set.empty


serviceName :: String
serviceName = "peer-manager"

multicastManager :: ProcessId -> Backend -> Process ()
multicastManager parent backend =
    forever $ do
        peers <- liftIO $ findPeers backend 1000000
        say $ "Multicast: got peers " ++ show (length peers)
        send parent $ DiscoveredPeers peers


sendHello :: NodeId -> [NodeId] -> NodeId -> Process ()
sendHello me myPeers nodeId = nsendRemote nodeId serviceName $ HelloRemote me myPeers

loop :: State -> Process State
loop state = do
    newState <- receiveWait [ match $ getPeersHandler        state
                            , match $ discoveredPeersHandler state
                            , match $ helloRemoteHandler     state
                            ]
    loop newState

peerManager :: [NodeId] -> Bool -> Backend -> Process ()
peerManager knownPeers useMulticast backend = do
    self <- getSelfPid
    me <- getSelfNode
    register serviceName self

    when useMulticast $ void $ spawnLocal $ multicastManager self backend

    forM_ knownPeers $ \peer -> nsendRemote peer serviceName $ DiscoveredPeers (me:knownPeers)
    -- sendHello me (me:knownPeers)
    spawnLocal $ broadcastPeersListProcess me

    void $ loop $ State $ Set.fromList (me:knownPeers)

getPeersHandler :: State -> GetPeers -> Process State
getPeersHandler state (GetPeers replyTo) = do
    send replyTo $ GetPeersReply $ Set.toAscList $ state ^. peers
    return state

broadcastPeersListProcess :: NodeId -> Process ()
broadcastPeersListProcess me =
    forever $ do
        liftIO $ threadDelay 300000
        knownPeers <- getPeers
        forM_ knownPeers $ \knownPeers ->
            forM_ knownPeers $ \peer ->
                when (peer /= me) $ nsendRemote peer serviceName $ DiscoveredPeers knownPeers

discoveredPeersHandler :: State -> DiscoveredPeers -> Process State
discoveredPeersHandler state (DiscoveredPeers newPeers) = do
    -- say "Got some discoveries"
    me <- getSelfNode
    let knownPeers = state ^. peers
        newKnownPeers = Set.union knownPeers $ Set.fromList newPeers

    when (knownPeers /= newKnownPeers) $ do
        say $ "Got new peers: " ++ (show $ Set.size newKnownPeers) ++ " " ++ (show newKnownPeers)
        let newPeerList = Set.toAscList newKnownPeers
        forM_ newKnownPeers $ \peer ->
            when (peer /= me) $ nsendRemote peer serviceName $ DiscoveredPeers newPeerList

    return $ state & peers .~ newKnownPeers


helloRemoteHandler :: State -> HelloRemote -> Process State
helloRemoteHandler state (HelloRemote nodeId newPeers) = do
    say $ "Got hello from " ++ show nodeId
    discoveredPeersHandler state (DiscoveredPeers newPeers)



getPeers :: Process (Maybe [NodeId])
getPeers = do
    pm <- whereis serviceName
    forM pm $ \pm -> do
        self <- getSelfPid
        send pm $ GetPeers self
        receiveWait [match $ \(GetPeersReply nodes) -> return nodes]
