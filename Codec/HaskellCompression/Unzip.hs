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

unzipit = via (\xs -> let (headxs,tailxs) = splitAt (fromIntegral startingLength) ( qmf "l0" (booleanListToIntegers startingLength  {- &&& id -} )  xs) in if xs == [] then [] else unzipit' initdb3 headxs tailxs)

unzipit' :: Map.Bimap [[Bool]] Int -> [Bool] -> [Bool] -> [Bool]
unzipit' library buffer xs = let
	librarySize = Map.size library
        keyLength = qt$  boolsRequiredForInteger . (+1) $ librarySize 
	(headxs,tailxs) = splitAt (fromIntegral keyLength) xs
	Just key = booleanListToInteger buffer `Map.lookupR` library
	ref = fromJust $ if (booleanListToInteger headxs) == librarySize then Just key else qt (booleanListToInteger headxs) `Map.lookupR` library
	in if length headxs < (fromIntegral keyLength) then concat $ map (padBooleanList 8. takeLast 8) $ library Map.!> booleanListToInteger buffer
                                                       else case Map.lookup [pruneBooleanList buffer,pruneBooleanList headxs] library of
	  Just n -> unzipit' library (integerToBooleanList n) tailxs
	  Nothing -> (concat $ map ( padBooleanList 8 . takeLast 8) key) ++ (unzipit' ((Map.insert (qt$ map pruneBooleanList $ concat $ [key ,take 1 ref])) librarySize library) headxs tailxs)


