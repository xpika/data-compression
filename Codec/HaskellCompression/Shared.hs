module Codec.HaskellCompression.Shared where

import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))

startingLength = fromIntegral 9 
viaBool f ns = int8Chunks (f (toBoolean8s ns))

lengthOfKeys = (2 ^ (startingLength - 1)) - 1

initdb :: Map.Bimap [[Bool]] Int
initdb = Map.fromList (Data.List.zipWith (\x y ->([x],y)) (integersToBooleanListsPadded startingLength [0..lengthOfKeys]) [0..])


initdb3 :: Map.Bimap [[Bool]] Int
initdb3 = Map.fromList (Data.List.zipWith (\x y ->([x],y)) (integersToBooleanLists [0..lengthOfKeys]) [0..])

initdb2 :: Map.Bimap (Maybe Int,Int) Int
initdb2 = Map.fromList (Data.List.zipWith (\x y ->((Nothing,fromIntegral x),fromIntegral y)) [0..lengthOfKeys] [0..])

via = viaNum . viaBool
