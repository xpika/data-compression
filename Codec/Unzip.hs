module Codec.Unzip
where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow
import qualified Codec.Zip


viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))
lengthOfKeys = 127
things xs = (head xs,tail xs)

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
