module Codec.HaskellCompression.Zip where

import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

import Codec.HaskellCompression.Shared

zipit :: B.ByteString -> B.ByteString
zipit = via (\xs -> let (headxs,tailxs) = things xs in if xs == [] then [] else zipit' initdb headxs tailxs)
  
zipit' :: Map.Bimap [Bool] Int -> [Bool] -> [Bool] -> [Bool]
zipit' library buffer xs = let
  (headxs,tailxs) = things xs
  key = buffer++headxs
  in if xs == [] then buffer else case Map.lookup key library of
     Just n -> zipit' library (integerToBooleanListPadded 8 n) tailxs
     _ -> buffer ++  zipit' (Map.insert key (Map.size library) library) headxs tailxs