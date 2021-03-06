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
                                                                     expectTimeout,
                                                                     getSelfPid,
                                                                     match,
                                                                     receiveTimeout,
                                                                     receiveWait,
                                                                     say, send,
                                                                     spawnLocal)
import           Control.Distributed.Process.Backend.SimpleLocalnet (Backend,
                                                                     findPeers,
                                                                     initializeBackend,
                                                                     newLocalNode)
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Control.Exception
import           Control.Lens
import           Control.Monad                                      (forM_,
                                                                     void)
import           Control.Monad.Trans                                (liftIO)
import qualified Data.ByteString.Char8                              as BS
import           GHC.IO.Exception
import           Numeric                                            (showFFloat)
import           Options
import           System.IO
import           System.Random                                      (mkStdGen)

import           Broadcaster                                        (broadcaster)
import           Broadcaster.API                                    (BroadcastFinished (..),
                                                                     FinishBroadcasting (..))
import           Collector                                          (FinalResult (..),
                                                                     FinalizeRequest (..),
                                                                     PrecomputeResultRequest (..),
                                                                     collector)
import           PeerManager                                        (getPeers,
                                                                     peerManager)

data MainOptions = MainOptions { _sendFor                :: Int
                               , _waitFor                :: Int
                               , _drngSeed               :: Int
                               , _connectFor             :: Int
                               , _host                   :: String
                               , _port                   :: Int
                               , _peersFile              :: Maybe String
                               , _multicastDiscovery     :: Bool
                               , _bufferLengthMultiplier :: Int
                               , _sendDelay              :: Int
                               } deriving (Show)

makeLenses ''MainOptions

instance Options MainOptions where
    defineOptions = pure MainOptions
        <*> simpleOption "send-for" 10
            "How many seconds the system sends the messages."
        <*> simpleOption "wait-for" 10
            "How many seconds the system waits for undelivered messages."
        <*> simpleOption "with-seed" 1
            "Random number generator seed."
        <*> simpleOption "connect-for" 0
            "Time to discover other peers."
        <*> simpleOption "host" "127.0.0.1"
            "Hostname for local node."
        <*> simpleOption "port" 10000
            "Port for local node."
        <*> simpleOption "peers-file" Nothing
            "File with known peers list (in host:port per line format)."
        <*> simpleOption "multicast-discovery" False
            "Whether to use multicast discovery in addition to peer exchange."
        <*> simpleOption "buffer-mult" 4
            "Buffer length multiplier (1 = buffer up to 10 000 messages)."
        <*> simpleOption "send-delay" 10000
            "Delay between messages in microseconds. Use this to control throughput."

sleepSeconds :: Int -> Process ()
sleepSeconds t = sleepMilis $ 1000 * t

sleepMilis :: Int -> Process ()
sleepMilis t = liftIO $ threadDelay $ 1000 * t

makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]


handleReadError :: IOException -> IO String
handleReadError e = do -- stderr not synchronized with `say`, output garbled is possible
  hPutStrLn stderr "Can't read peer list"
  hPutStrLn stderr $ "errno: " ++ show (ioe_errno e)
  hPutStrLn stderr $ "description: " ++ ioe_description e
  return "" -- empty node list

readPeers :: Maybe String -> Process [NodeId]
readPeers Nothing = return []
readPeers (Just path) = do
    contents <- liftIO $ readFile path `catch` handleReadError
    return $ makeNodeId <$> lines contents

mainProcess :: MainOptions -> Backend -> Process ()
mainProcess opts backend = do
    mainPid <- getSelfPid

    collectorPid <- spawnLocal $ collector mainPid (opts ^. bufferLengthMultiplier)

    say "Discovering peers"
    knownPeers <- readPeers $ opts ^. peersFile
    spawnLocal $ peerManager knownPeers (opts ^. multicastDiscovery) backend

    sleepSeconds $ opts ^. connectFor

    knownPeers <- getPeers
    say $ "Starting broadcast, at this point number of known peers is " ++ show (length knownPeers)
    let stdGen = mkStdGen $ opts ^. drngSeed
    broadcasterPid <- spawnLocal $ broadcaster stdGen (opts ^. sendDelay)

    sleepSeconds $ opts ^. sendFor

    say "Finish broadcast"
    send broadcasterPid FinishBroadcasting

    spawnLocal $ do
        let precomputeRequestCount = (opts ^. waitFor) * 4 - 2

        forM_ [1..precomputeRequestCount] $ \_ -> do
            send collectorPid PrecomputeResultRequest
            sleepMilis 250

        send collectorPid FinalizeRequest

    mayResult <- expectTimeout (1000000 * opts ^. waitFor) :: Process (Maybe FinalResult)

    liftIO $ putStrLn $ case mayResult of
        Just (FinalResult (count, score)) ->  "(" ++ show count ++ ", " ++ Numeric.showFFloat Nothing score "" ++ ")"
        Nothing             ->  "No result. Timed out"

    sleepSeconds 1 -- wait for stderr buffers to flush

main :: IO ()
main = runCommand $ \opts _args -> do

    backend <- initializeBackend (opts ^. host) (show $ opts ^. port) initRemoteTable
    node    <- newLocalNode backend

    runProcess node $ mainProcess opts backend
