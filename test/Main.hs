{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Distributed.Process (NodeId (..))
import qualified Data.ByteString.Char8       as BS
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Time.Clock
import           Network.Transport           (EndPointAddress (..))


import qualified Collector                   as C
import           Message

makeNodeId :: String -> NodeId
makeNodeId addr = NodeId . EndPointAddress . BS.concat $ [BS.pack addr, ":0"]

messages :: UTCTime -> Set Message
messages baseTime = Set.fromList [ Message (makeNodeId "1") 0 1.0                         baseTime
                                 , Message (makeNodeId "1") 1 1.0 $ addUTCTime nominalDay baseTime
                                 , Message (makeNodeId "1") 2 1.0 $ addUTCTime nominalDay $ addUTCTime nominalDay baseTime
                                 , Message (makeNodeId "2") 0 0.5 $ addUTCTime nominalDay $ addUTCTime nominalDay $ addUTCTime nominalDay baseTime
                                 , Message (makeNodeId "1") 3 1.0 $ addUTCTime nominalDay $ addUTCTime nominalDay $ addUTCTime nominalDay $ addUTCTime nominalDay baseTime
                                 ]

main :: IO ()
main = do
    currentTime <- getCurrentTime
    let msgs = messages currentTime
    print msgs
    print $ (length msgs, C.computeScore msgs)
    print $ C.computeScore' msgs (0, 0.0)
