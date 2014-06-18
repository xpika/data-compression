module Codec.Unzip
where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

viaBool f ns = int8Chunks (f (toBoolean8s ns))

viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))

lengthOfKeys = 127
things xs = (head xs,tail xs)

things2 xs = (take 8 xs,drop 8 xs)

unzipit = viaNum (viaBool unzipit')

initdb = Map.fromList (Data.List.zipWith (\x y ->(x,y)) (integersToPaddedBooleansLists 8 [0..lengthOfKeys]) [0..] )

unzipit' :: [Bool] -> [Bool]
unzipit' xs = if xs == [] then [] else  unzipit'' initdb headxs tailxs
  where (headxs,tailxs) = things2 xs

unzipit'' :: Map.Bimap [Bool] Int -> [Bool] -> [Bool] -> [Bool]
unzipit'' library buffer xs = let 
	(headxs,tailxs) = things2 xs
	Just key = (booleanListToInteger buffer) `Map.lookupR` library
 	librarySize = Map.size library 
	ref =  fromJust (if ((booleanListToInteger headxs) ==  librarySize) then Just key else (booleanListToInteger headxs) `Map.lookupR` library) 
	in if xs == [] then library Map.!> (booleanListToInteger buffer)
				   else case Map.lookup (buffer++headxs) library of
	  Just n ->  unzipit'' library (integerToBooleanListPadded 8 n) tailxs
	  _ -> (key) ++ unzipit'' (Map.insert (key ++ (take 8 ref)) librarySize library) headxs tailxs 