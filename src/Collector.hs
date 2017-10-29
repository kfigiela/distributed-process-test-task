{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Collector where
import           GHC.Generics

import           Control.Distributed.Process (Process, getSelfPid, match,
                                              receiveWait, register, say)
import           Control.Lens
import           Control.Monad               (forM_, forever, void)
import           Control.Monad.Trans         (lift, liftIO)


import           Data.Binary.Orphans         (Binary)
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Time.Clock             (UTCTime, getCurrentTime)
import qualified Data.Time.Clock             as Clock
import           Data.Typeable               (Typeable)

import           Message

whileRightM :: Monad m => Either b a -> (a -> m (Either b a)) -> m b
whileRightM (Left result) action = return result
whileRightM (Right state) action = action state >>= flip whileRightM action

newtype State = State { _messages :: Set Message } deriving (Generic, Typeable, Binary, Show)

makeLenses ''State

initState = State Set.empty

data FinishRequest = FinishRequest deriving (Generic, Typeable, Binary, Show)

type Result = (Int, Double)

computeScore :: State -> Double
computeScore State { _messages = messages } = snd $ Set.foldr' f (1, 0.0) messages where
    f Message { _value = value} (ix, acc) = (ix + 1, acc + ix * value)

computeResult :: State -> Result
computeResult state@State { _messages = messages } = (Set.size messages, computeScore state)

processMessage :: State -> Message -> Process (Either Result State)
processMessage state message = return $ Right $ state & messages %~ Set.insert message

processFinish :: State -> FinishRequest -> Process (Either Result State)
processFinish state message = do
    let result = computeResult state
    say $ show result
    return $ Left result

collector :: Process ()
collector = void $ do
    collectorPid <- getSelfPid
    register "collector" collectorPid

    whileRightM (Right initState) (\state ->
            receiveWait [ match $ processFinish state
                        , match $ processMessage state
                        ]
        )
