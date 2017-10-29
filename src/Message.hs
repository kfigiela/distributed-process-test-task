{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Message where
import           GHC.Generics

import           Control.Distributed.Process (NodeId)
import           Control.Lens
import           Data.Binary.Orphans         (Binary)
import           Data.Monoid                 ((<>))
import           Data.Ord                    (comparing)
import           Data.Time.Clock             (UTCTime)
import           Data.Typeable               (Typeable)

data Message = Message { _source :: NodeId, _sequenceNumber :: Int, _value :: Double, _timestamp :: UTCTime} deriving (Generic, Typeable, Binary, Show, Eq)

makeLenses ''Message

instance Ord Message where
    compare = comparing (view timestamp) <> comparing (view source) <> comparing (view sequenceNumber)
