{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process                        (Process,
                                                                     exit, say,
                                                                     send,
                                                                     spawnLocal)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Node                   (initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad.Trans                                (liftIO)
import           System.Random                                      (mkStdGen)

import           Broadcaster                                        (FinishBroadcasting (..),
                                                                     broadcaster)
import           Collector                                          (FinishRequest (..),
                                                                     collector)
-- import           Control.Applicative
import           Options

data MainOptions = MainOptions { _sendFor    :: Int
                               , _waitFor    :: Int
                               , _drngSeed   :: Int
                               , _connectFor :: Int
                               , _host       :: String
                               , _port       :: Int
                               }

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

sleepSeconds :: Int -> Process ()
sleepSeconds t = liftIO $ threadDelay $ t * 1000000

main :: IO ()
main = runCommand $ \opts _args -> do
    let stdGen = mkStdGen $ opts ^. drngSeed + opts ^. port

    be <- initializeBackend (opts ^. host) (show $ opts ^. port) initRemoteTable
    node <- newLocalNode be

    runProcess node $ do
        collectorPid <- spawnLocal collector
        sleepSeconds $ opts ^. connectFor
        peers <- liftIO $ findPeers be (1000000 * opts ^. connectFor)
        -- liftIO $ threadDelay $ (opts ^. connect)
        say $ "got peers " ++ (show $ length peers)
        broadcasterPid <- spawnLocal $ broadcaster peers (show $ opts ^. port) stdGen
        spawnLocal $ do
            sleepSeconds $ opts ^. sendFor + opts ^. waitFor
            send collectorPid FinishRequest
            say "Finish collecting"
        -- spawnLocal $ do
        sleepSeconds $ opts ^. sendFor
        send broadcasterPid FinishBroadcasting
        say "broadcaster done"

        sleepSeconds $ 1 + opts ^. waitFor

