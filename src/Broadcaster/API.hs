{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Broadcaster.API where

import           GHC.Generics

import           Control.Distributed.Process (NodeId)
import           Data.Typeable               (Typeable)

import           Data.Binary.Orphans         (Binary)



data    FinishBroadcasting = FinishBroadcasting          deriving (Generic, Typeable, Binary)
newtype BroadcastFinished  = BroadcastFinished  Int      deriving (Generic, Typeable, Binary)
newtype NewPeerList        = NewPeerList        [NodeId] deriving (Generic, Typeable, Binary)
