module Codec.HaskellCompression 
where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))

lengthOfKeys = 127
things xs = (head xs,tail xs)
zipit = viaNum zipitNew
zipitNew = zipit' 
zipit' xs = (if xs' == [] then [] else zipit'' (Map.fromList (zip (Data.List.map ((:[])) [0..lengthOfKeys]) [0..])) headxs tailxs)
  where (headxs,tailxs) = things xs'
        xs' = xs

zipit'' library buffer xs = let 
  (headxs,tailxs) = things xs
  key = [buffer,headxs]
  in if xs == [] then [buffer] else case Map.lookup key library of
     Just n -> zipit'' library n tailxs
     _ -> [buffer] ++ zipit'' (Map.insert key (Map.size library) library) headxs tailxs

unzipit = viaNum unzipit'
unzipit' xs = if xs == [] then [] else unzipit'' (Map.fromList (zip (Data.List.map ((:[])) [0..lengthOfKeys]) [0..])) (head xs) (tail xs)
unzipit'' library buffer xs = let 
   (headxs,tailxs) = things xs
   Just key = buffer `Map.lookupR` library
   librarySize = Map.size library 
   ref = fromJust (if headxs == librarySize then Just key else headxs `Map.lookupR` library)
   in if xs == [] then library Map.!> buffer
                  else case Map.lookup [buffer,headxs] library of
      Just n ->  unzipit'' library n tailxs
      _ -> key ++ unzipit'' (Map.insert (key ++ (take 1 ref)) librarySize library) headxs tailxs
