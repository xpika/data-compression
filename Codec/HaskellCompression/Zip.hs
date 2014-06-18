module Codec.HaskellCompression.Zip where

import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))

viaBool f ns = int8Chunks (f (toBoolean8s ns))

lengthOfKeys = 127
things xs = (take 8 xs,drop 8 xs)

zipit :: B.ByteString -> B.ByteString
zipit = viaNum (viaBool zipit')

initdb = Map.fromList (Data.List.zipWith (\x y ->(x,y)) (integersToPaddedBooleansLists 8 [0..lengthOfKeys]) [0..] )

zipit' :: [Bool] -> [Bool]
zipit' xs = (if xs == [] then [] else zipit'' initdb headxs tailxs)
  where (headxs,tailxs) = things xs
  
zipit'' :: Map.Bimap [Bool] Int -> [Bool] -> [Bool] -> [Bool]
zipit'' library buffer xs = let
  (headxs,tailxs) = things xs
  key = buffer++headxs
  in if xs == [] then buffer else case Map.lookup key library of
     Just n -> zipit'' library (integerToBooleanListPadded 8 n) tailxs
     _ -> buffer ++  zipit'' (Map.insert key (Map.size library) library) headxs tailxs
