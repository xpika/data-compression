module Codec.HaskellCompression.Unzip where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow
import Codec.HaskellCompression.Shared

unzipit = via (\xs -> let (headxs,tailxs) = things xs in if xs == [] then [] else  unzipit' initdb headxs tailxs)

unzipit' :: Map.Bimap [Bool] Int -> [Bool] -> [Bool] -> [Bool]
unzipit' library buffer xs = let
	(headxs,tailxs) = things xs
	Just key = (booleanListToInteger buffer) `Map.lookupR` library
 	librarySize = Map.size library 
	ref =  fromJust (if ((booleanListToInteger headxs) ==  librarySize) then Just key else (booleanListToInteger headxs) `Map.lookupR` library) 
	in if xs == [] then library Map.!> (booleanListToInteger buffer)
				   else case Map.lookup (buffer++headxs) library of
	  Just n ->  unzipit' library (integerToBooleanListPadded 8 n) tailxs
	  _ -> (key) ++ unzipit' (Map.insert (key ++ (take 8 ref)) librarySize library) headxs tailxs 