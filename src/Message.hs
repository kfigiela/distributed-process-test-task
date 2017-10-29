{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Message where
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


data Message = Message { source :: ProcessId, sequence :: Int, value :: Double, timestamp :: UTCTime} deriving (Generic, Typeable, Binary, Show)
