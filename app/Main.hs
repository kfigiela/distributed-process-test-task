{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Main where
import           GHC.Generics

import qualified Control.Distributed.Backend.P2P          as P2P
import           Control.Distributed.Process              (NodeId, Process,
                                                           expect, getSelfPid,
                                                           nsendRemote,
                                                           register, say)
import           Control.Distributed.Process.Node         (forkProcess,
                                                           initRemoteTable,
                                                           runProcess)
import           Control.Monad                            (forM_, forever,
                                                           mapM_)
import           Data.Time.Clock                          (UTCTime,
                                                           getCurrentTime)
import qualified Data.Time.Clock                          as Clock
import           System.Environment                       (getArgs)

import           Control.Concurrent                       (threadDelay)
import           Control.Distributed.Process.Serializable (Serializable)
import           Control.Monad.Random                     (Random, getRandom)
import           Control.Monad.Trans                      (lift, liftIO)
import           Control.Monad.Trans.Random.Strict        (RandT, evalRandT)
import           Data.Binary.Orphans                      (Binary)
import           Data.Typeable                            (Typeable)
import           System.Random                            (StdGen, mkStdGen)

readPeerList :: String -> IO [NodeId]
readPeerList filename = do
    contents <- readFile filename
    return $ P2P.makeNodeId <$> lines contents

data Message = Message { timestamp :: UTCTime, value :: Double} deriving (Generic, Typeable, Binary, Show)

buildMessage :: RandT StdGen Process Message
buildMessage = do
    r <- getRandom
    currentTime <- liftIO getCurrentTime
    return $ Message currentTime r

sender :: String -> StdGen -> Process ()
sender port g = flip evalRandT g $ do
        lift $ say ("hello " ++ port)
        forever $ do
            peers <- lift P2P.getPeers
            lift $ say $ "known peers: " ++ show (length peers)
            msg <- buildMessage
            forM_ peers $ \peer -> lift $ do
                say $ port ++ " => " ++ show peer
                nsendRemote peer "echo-server" msg
            liftIO $ threadDelay 2000000

receiver :: Process ()
receiver = do
    echoServerPid <- getSelfPid
    register "echo-server" echoServerPid

    forever $ do
        msg <- expect :: Process Message
        say $ show msg
        return 0

main = do
    [host, port, peersFile] <- getArgs

    peerNodeIds <- readPeerList peersFile
    let stdGen = mkStdGen 1

    (node, pid) <- P2P.bootstrapNonBlocking host port peerNodeIds initRemoteTable $ say "Controller started"

    echoServerPid <- forkProcess node receiver
    liftIO $ threadDelay 2000000
    runProcess node $ P2P.waitController $ sender (show port) stdGen
