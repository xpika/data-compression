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
zipit = via (\xs -> let (headxs,tailxs) = splitAt 8 xs in if xs == [] then [] else  qg "outy" (length &&& integerChunks 8 &&& integerChunks 9 &&& id) $ zipit' initdb headxs tailxs)

zipit' :: Map.Bimap [Bool] Int -> [Bool] -> [Bool] -> [Bool]
zipit' library buffer xs = let
  (headxs,tailxs) = splitAt 8 xs
  key = buffer ++ headxs
  librarySize = Map.size library
  keyLength = ceiling . logBase 2. fromIntegral. (+1) $ librarySize
  in  if xs == [] then padBoolean keyLength buffer else case Map.lookup key library of
    Just n -> zipit' library (integerToBooleanListPadded keyLength n) tailxs
    _ -> (qd (buffer,keyLength) $ padBoolean 9 buffer) ++ zipit' (Map.insert key librarySize library) headxs tailxs
	
