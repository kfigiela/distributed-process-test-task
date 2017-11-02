{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Broadcaster.API where

import           GHC.Generics

import           Control.Distributed.Process (NodeId)
import           Data.Binary.Orphans         (Binary)
import           Data.Typeable               (Typeable)

data    FinishBroadcasting = FinishBroadcasting          deriving (Generic, Typeable, Binary)
newtype BroadcastFinished  = BroadcastFinished  Int      deriving (Generic, Typeable, Binary)
newtype NewPeerList        = NewPeerList        [NodeId] deriving (Generic, Typeable, Binary)
