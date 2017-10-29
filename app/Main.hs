{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where
import           GHC.Generics


import           Control.Distributed.Process                        (NodeId,
                                                                     Process,
                                                                     ProcessId,
                                                                     exit,
                                                                     expect,
                                                                     getSelfPid,
                                                                     match,
                                                                     nsendRemote,
                                                                     receiveWait,
                                                                     register,
                                                                     say, send,
                                                                     spawnLocal)
import           Control.Distributed.Process.Node                   (forkProcess,
                                                                     initRemoteTable,
                                                                     runProcess)
import           Control.Lens
import           Control.Monad                                      (forM_,
                                                                     forever,
                                                                     mapM_,
                                                                     void)
import           Data.Time.Clock                                    (UTCTime, getCurrentTime)
import qualified Data.Time.Clock                                    as Clock
import           System.Environment                                 (getArgs)

import           Control.Concurrent                                 (threadDelay)
import           Control.Distributed.Process.Backend.SimpleLocalnet
import           Control.Distributed.Process.Serializable           (Serializable)
-- import           Control.Monad.Random                               (Random,
--                                                                      getRandom)
import           Control.Monad.Trans                                (lift,
                                                                     liftIO)
import           Control.Monad.Trans.Random.Strict                  (RandT,
                                                                     evalRandT)
import           Data.Binary.Orphans                                (Binary)
import           Data.Typeable                                      (Typeable)
import           System.Random                                      (StdGen,
                                                                     mkStdGen,
                                                                     randoms)

import           Broadcaster                                        (broadcaster)
import           Collector                                          (FinishRequest (..),
                                                                     collector)
import           Control.Monad.Loops                                (iterateUntilM)
import           Data.Maybe                                         (isNothing)
-- -- readPeerList :: String -> IO [NodeId]
-- -- readPeerList filename = do
-- --     contents <- readFile filename
-- --     return $ P2P.makeNodeId <$> lines contents

-- data Message = Message { source :: ProcessId, sequence :: Int, value :: Double, timestamp :: UTCTime} deriving (Generic, Typeable, Binary, Show)

-- -- buildMessage :: RandT StdGen Process Message
-- -- buildMessage = do
-- --     r <- getRandom
-- --     currentTime <- liftIO getCurrentTime
-- --     return $ Message currentTime 0 r -- TODO: sequence number

-- whileRightM :: Monad m => Either b a -> (a -> m (Either b a)) -> m b
-- whileRightM (Left result) action = return result
-- whileRightM (Right state) action = action state >>= flip whileRightM action


-- sender :: [NodeId] -> String -> StdGen -> Process ()
-- sender peers port g = do
--         say ("hello " ++ port)
--         self <- getSelfPid
--         let values = randoms g
--             messages = Message self <$> [0..] <*> values

--         forM_ messages $ \msg -> do
--             -- say $ "known peers: " ++ show (length peers)
--             liftIO $ threadDelay 1000
--             currentTime <- liftIO getCurrentTime
--             let msg' = msg currentTime
--             forM_ peers $ \peer -> nsendRemote peer "echo-server" msg'

-- newtype ReceiverState = ReceiverState { _messages :: [Message] } deriving (Generic, Typeable, Binary, Show)

-- makeLenses ''ReceiverState

-- initState = ReceiverState []

-- data FinishRequest = FinishRequest deriving (Generic, Typeable, Binary, Show)

-- processMessage :: ReceiverState -> Message -> Process (Either Int ReceiverState)
-- processMessage state message = do
--     return $ Right $ state & messages %~ (message:)

-- processFinish :: ReceiverState -> FinishRequest -> Process (Either Int ReceiverState)
-- processFinish state message = do
--     say $ show $ length $ state ^. messages
--     return $ Left $ length $ state ^. messages

-- receiver :: Process ()
-- receiver = void $ do
--     echoServerPid <- getSelfPid
--     register "echo-server" echoServerPid

--     whileRightM (Right initState) (\state ->
--             receiveWait [ match $ processFinish state
--                         , match $ processMessage state
--                         ]
--         )

main = do
    [host, port, peersFile] <- getArgs
    liftIO $ threadDelay 2000000
    let stdGen = mkStdGen 1

    be <- initializeBackend host port initRemoteTable
    node <- newLocalNode be
    peers <- findPeers be 1000000

    runProcess node $ do
        collectorPid <- spawnLocal collector
        liftIO $ threadDelay 2000000
        broadcasterPid <- spawnLocal $ broadcaster peers (show port) stdGen
        spawnLocal $ do
            liftIO $ threadDelay 20000000
            send collectorPid FinishRequest
            say "Finish collecting"
        -- spawnLocal $ do
        liftIO $ threadDelay 10000000
        exit broadcasterPid "done"
        say "broadcaster done"

        liftIO $ threadDelay 1000000000

