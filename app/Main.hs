{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process                        (Process,
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
sleepSeconds t = liftIO $ threadDelay $ 1000000 * t

mainProcess :: MainOptions -> Backend -> Process ()
mainProcess opts backend = do
    let stdGen = mkStdGen $ opts ^. drngSeed

    collectorPid <- spawnLocal collector
    sleepSeconds $ opts ^. connectFor
    peers <- liftIO $ findPeers backend (1000000 * opts ^. connectFor)
    -- liftIO $ threadDelay $ (opts ^. connect)
    say $ "got peers " ++ show (length peers)
    broadcasterPid <- spawnLocal $ broadcaster peers (show $ opts ^. port) stdGen
    mainPid <- getSelfPid

    sleepSeconds $ opts ^. sendFor

    send broadcasterPid $ FinishBroadcasting mainPid
    receiveWait [match $ \(BroadcastFinished sentMsgs) -> say $ "Sent messages: " ++ show sentMsgs]

    sleepSeconds $  opts ^. waitFor

    say "Finish collecting"
    send collectorPid $ FinishRequest mainPid
    receiveWait [match $ \(FinishResponse result) -> say $ "Final result: " ++ show result]

    say "All done"

main :: IO ()
main = runCommand $ \opts _args -> do

    backend <- initializeBackend (opts ^. host) (show $ opts ^. port) initRemoteTable
    node    <- newLocalNode backend

    runProcess node $ mainProcess opts backend
