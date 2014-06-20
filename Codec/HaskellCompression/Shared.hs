module Codec.HaskellCompression.Shared where

import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))

viaBool f ns = int8Chunks (f (toBoolean8s ns))

lengthOfKeys = 255

initdb :: Map.Bimap [Bool] Int
initdb = Map.fromList (Data.List.zipWith (\x y ->(x,y)) (integersToBooleanListsPadded 8 [0..lengthOfKeys]) [0..])

via = viaNum . viaBool