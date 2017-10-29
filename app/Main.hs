{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

import           Control.Concurrent                                 (threadDelay)
import           Network.Transport                                  (EndPointAddress (..))

import           Control.Distributed.Process                        (NodeId (..),
                                                                     Process,
                                                                     exit,
                                                                     getSelfPid,
                                                                     match,
                                                                     receiveWait,
                                                                     say, send,
                                                                     spawnLocal)
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend,
                                                                     findPeers,
                                                                     initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad.Trans                                (liftIO)
import           System.Random                                      (mkStdGen)

import           Broadcaster                                        (BroadcastFinished (..),
                                                                     FinishBroadcasting (..),
                                                                     broadcaster)
import           Collector                                          (FinishRequest (..),
                                                                     FinishResponse (..),
                                                                     collector)
import qualified Data.ByteString.Char8                              as BS

import           Options
import           PeerManager                                        (peerManager)

data MainOptions = MainOptions { _sendFor            :: Int
                               , _waitFor            :: Int
                               , _drngSeed           :: Int
                               , _connectFor         :: Int
                               , _host               :: String
                               , _port               :: Int
                               , _peersFile          :: Maybe String
                               , _multicastDiscovery :: Bool
                               } deriving (Show)

makeLenses ''MainOptions

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "send-for" 10
            "How many seconds the system sends the messages."
        <*> simpleOption "wait-for" 10
            "How many seconds the system waits for undelivered messages."
        <*> simpleOption "with-seed" 1
            "Random number generator seed"
        <*> simpleOption "connect-for" 5
            "Time to discover other peers"
        <*> simpleOption "host" "127.0.0.1"
            "Hostname for local node"
        <*> simpleOption "port" 10000
            "Port for local node"
        <*> simpleOption "peers-file" Nothing
            "File with known peers list (in host:port per line format)"
        <*> simpleOption "multicast-discovery" False
            "Whether to use multicast discovery in addition to peer exchange"

sleepSeconds :: Int -> Process ()
sleepSeconds t = liftIO $ threadDelay $ 1000000 * t


makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

readPeers :: Maybe String -> Process [NodeId]
readPeers Nothing = return []
readPeers (Just path) = do
    contents <- liftIO $ readFile path
    return $ makeNodeId <$> lines contents

mainProcess :: MainOptions -> Backend -> Process ()
mainProcess opts backend = do
    let stdGen = mkStdGen $ opts ^. drngSeed
    mainPid <- getSelfPid

    knownPeers <- readPeers $ opts ^. peersFile

    collectorPid <- spawnLocal collector

    say "Discovering peers"
    spawnLocal $ peerManager knownPeers (opts ^. multicastDiscovery) backend

    sleepSeconds $ opts ^. connectFor

    say "Starting broadcast"
    broadcasterPid <- spawnLocal $ broadcaster stdGen

    sleepSeconds $ opts ^. sendFor

    send broadcasterPid $ FinishBroadcasting mainPid
    receiveWait [match $ \(BroadcastFinished sentMsgs) -> say $ "Sent messages: " ++ show sentMsgs]

    sleepSeconds $  opts ^. waitFor

    say "Finish collecting"
    send collectorPid $ FinishRequest mainPid
    receiveWait [match $ \(FinishResponse result) -> say $ "Final result: " ++ show result]

    -- say "All done"
    sleepSeconds 1 -- wait for stderr buffers to flush

main :: IO ()
main = runCommand $ \opts _args -> do

    backend <- initializeBackend (opts ^. host) (show $ opts ^. port) initRemoteTable
    node    <- newLocalNode backend

    runProcess node $ mainProcess opts backend
