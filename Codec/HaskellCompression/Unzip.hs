module Codec.HaskellCompression.Unzip where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow
import Codec.HaskellCompression.Shared
import QuickTrace
import Debug.Trace

unzipit = via (\xs -> let (headxs,tailxs) = splitAt 9 xs in if xs == [] then [] else unzipit' initdb headxs tailxs)

takeLast n xs = drop (length xs-n) xs

unzipit' :: Map.Bimap [Bool] Int -> [Bool] -> [Bool] -> [Bool]
unzipit' library buffer xs = let
	librarySize = Map.size library
	bitsForLibarySize size = 9 -- qd "bs" $ ceiling . logBase 2. fromIntegral. (+1) $ size
	(headxs,tailxs) = splitAt (bitsForLibarySize librarySize) xs
	Just key = booleanListToInteger buffer `Map.lookupR` library
	ref = fromJust $ if (booleanListToInteger headxs) == librarySize then Just key else booleanListToInteger headxs `Map.lookupR` library
	in if length (take (bitsForLibarySize librarySize) xs) < bitsForLibarySize librarySize then library Map.!> (booleanListToInteger buffer)
				   else case Map.lookup (buffer++headxs) library of
	  Just n -> unzipit' library (integerToBooleanListPadded 8 n) tailxs
	  _ -> key ++ (unzipit' (Map.insert (key ++ ref) librarySize library) headxs tailxs)