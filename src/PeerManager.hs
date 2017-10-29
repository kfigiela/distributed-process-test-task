{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module PeerManager (peerManager, getPeers) where

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
newtype HelloRemote = HelloRemote [NodeId]  deriving (Generic, Typeable, Binary)

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
        say $ "got peers " ++ show (length peers)
        send parent $ DiscoveredPeers peers


sendHello :: NodeId -> [NodeId] -> Process ()
sendHello nodeId myPeers = nsendRemote nodeId serviceName $ HelloRemote myPeers

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

    forM_ knownPeers $ flip sendHello (me:knownPeers)
    send self $ DiscoveredPeers (me:knownPeers)

    void $ loop initState

getPeersHandler :: State -> GetPeers -> Process State
getPeersHandler state (GetPeers replyTo) = do
    send replyTo $ GetPeersReply $ Set.toList $ state ^. peers
    return state

discoveredPeersHandler :: State -> DiscoveredPeers -> Process State
discoveredPeersHandler state (DiscoveredPeers newPeers) = do
    -- say "Got some discoveries"
    me <- getSelfNode
    let knownPeers = state ^. peers
        newKnownPeers = Set.union knownPeers $ Set.fromList newPeers

    when (knownPeers /= newKnownPeers) $ do
        say $ "Got new peers: old " ++ (show $ Set.size knownPeers) ++ " vs new " ++ (show $ Set.size newKnownPeers)
        let newPeerList = Set.toList newKnownPeers
        forM_ newKnownPeers $ \peer ->
            when (peer /= me) $ nsendRemote peer serviceName $ DiscoveredPeers newPeerList

    return $ state & peers .~ newKnownPeers


helloRemoteHandler :: State -> HelloRemote -> Process State
helloRemoteHandler state (HelloRemote newPeers) = discoveredPeersHandler state (DiscoveredPeers newPeers)



getPeers :: Process (Maybe [NodeId])
getPeers = do
    pm <- whereis serviceName
    forM pm $ \pm -> do
        self <- getSelfPid
        send pm $ GetPeers self
        receiveWait [match $ \(GetPeersReply nodes) -> return nodes]
