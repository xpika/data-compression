module Codec.HaskellCompression.Zip where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow
import Codec.HaskellCompression.Shared

import QuickTrace

zipit :: B.ByteString -> B.ByteString
zipit = via (\xs -> let (headxs,tailxs) = splitAt 8 xs in if xs == [] then [] else zipit' initdb2 (booleanListToInteger headxs) tailxs)

zipit' :: Map.Bimap (Maybe Int,Int) Int -> Int -> [Bool] -> [Bool]
zipit' library buffer xs = let
  librarySize =  Map.size library
  keyLength = boolsRequiredForInteger librarySize 
  (headxs,tailxs) = splitAt 8 xs
  key = (Just buffer, booleanListToInteger headxs)
  in if xs == [] then (integerToBooleanListPadded (fromIntegral keyLength) buffer) else case Map.lookup key library of
    Just n -> zipit' library n tailxs
    _ -> ( integerToBooleanListPadded (fromIntegral keyLength) buffer) ++ zipit' (Map.insert key librarySize library) (booleanListToInteger headxs) tailxs
