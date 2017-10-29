{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Broadcaster where
import           GHC.Generics


import           Control.Distributed.Process                        (NodeId,
                                                                     Process,
                                                                     ProcessId,
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

import           Control.Monad.Loops                                (iterateUntilM)
import           Data.Maybe                                         (isNothing)
import           Message                                            (Message (..))


broadcaster :: [NodeId] -> String -> StdGen -> Process ()
broadcaster peers port g = do
        say ("hello " ++ port)
        self <- getSelfPid
        let values = randoms g
            messages = Message self <$> [0..] <*> values

        forM_ messages $ \msg -> do
            -- say $ "known peers: " ++ show (length peers)
            liftIO $ threadDelay 1000
            currentTime <- liftIO getCurrentTime
            let msg' = msg currentTime
            forM_ peers $ \peer -> nsendRemote peer "collector" msg'
