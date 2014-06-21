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
zipit = via (\xs -> let (headxs,tailxs) = splitAt 9 xs in if xs == [] then [] else {- qmf "o" (const xs &&& id &&& length . id ) $ -} zipit' initdb headxs tailxs)

zipit' :: Map.Bimap [[Bool]] Int -> [Bool] -> [Bool] -> [Bool]
zipit' library buffer xs = let
  keyLength = 9
  (headxs,tailxs) = splitAt 8 xs
  key = [buffer,headxs]
  librarySize = Map.size library
  in if xs == [] then (padBooleanList 9 buffer) else case Map.lookup key library of
    Just n -> zipit' library (integerToBooleanListPadded keyLength n) tailxs
    _ -> (padBooleanList 9 buffer) ++ zipit' (Map.insert key librarySize library) headxs tailxs
	
