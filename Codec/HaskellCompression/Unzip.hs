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

takeLast n xs = drop (length xs-n) xs

unzipit = via (\xs -> let (headxs,tailxs) = splitAt 9 ( {- qmf "l0" id $ -} (xs)) in if xs == [] then [] else {- qmf "l" (const xs &&& id) $ -} unzipit' initdb headxs tailxs)

unzipit' :: Map.Bimap [[Bool]] Int -> [Bool] -> [Bool] -> [Bool]
unzipit' library buffer xs = let
	librarySize = Map.size library
	(headxs,tailxs) = splitAt 9 xs
	Just key = booleanListToInteger buffer `Map.lookupR` library
	ref = fromJust $ if (booleanListToInteger headxs) == librarySize then Just key else booleanListToInteger headxs `Map.lookupR` library
	in if length headxs < 9 then concat $ map (takeLast 8) $ library Map.!> booleanListToInteger buffer
			                else case Map.lookup [buffer,headxs] library of
	  Just n -> unzipit' library (integerToBooleanListPadded 9 n) tailxs
	  Nothing -> (concat $ map (take 8) key) ++ (unzipit' (Map.insert (key++(take 1 ref)) librarySize library) headxs tailxs)