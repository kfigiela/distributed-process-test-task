{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Collector where
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

import           Message

whileRightM :: Monad m => Either b a -> (a -> m (Either b a)) -> m b
whileRightM (Left result) action = return result
whileRightM (Right state) action = action state >>= flip whileRightM action

newtype State = State { _messages :: [Message] } deriving (Generic, Typeable, Binary, Show)

makeLenses ''State

initState = State []

data FinishRequest = FinishRequest deriving (Generic, Typeable, Binary, Show)

processMessage :: State -> Message -> Process (Either Int State)
processMessage state message = return $ Right $ state & messages %~ (message:)

processFinish :: State -> FinishRequest -> Process (Either Int State)
processFinish state message = do
    say $ show $ length $ state ^. messages
    return $ Left $ length $ state ^. messages

collector :: Process ()
collector = void $ do
    collectorPid <- getSelfPid
    register "collector" collectorPid

    whileRightM (Right initState) (\state ->
            receiveWait [ match $ processFinish state
                        , match $ processMessage state
                        ]
        )
