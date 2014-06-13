module Codec.HaskellCompression 
where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe

import Data.BooleanList

import qualified Data.ByteString as B (pack,unpack,ByteString)


viaNum f d = B.pack ( map fromIntegral (f (map fromIntegral (B.unpack d))))

lengthOfKeys = 127

zipit = viaNum zipit'
zipit' xs = if xs == [] then [] else zipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) (head xs) (tail xs)
zipit'' library buffer xs = let 
  key = [buffer,head xs]
  (output,buffer',library') = case Map.lookup key library of
     Just n -> ([],n,library)
     _ -> ( [buffer],head xs,Map.insert key (Map.size library) library)
  in if xs == [] then [buffer] else output ++ zipit'' library' buffer' (tail xs)

unzipit = viaNum unzipit'
unzipit' [] = []
unzipit' (x:xs) = unzipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) x xs
unzipit'' library buffer xs = let 
   headxs = head xs
   Just key = buffer `Map.lookupR` library 
   librarySize = Map.size library 
   ref = fromJust (if headxs == librarySize then Just key else headxs `Map.lookupR` library)
   ( output ,buffer',library') = case Map.lookup [buffer,headxs] library of
      Just n -> ( [] ,n ,library)
      _ -> (key, headxs ,Map.insert (key ++ (take 1 ref)) librarySize library )
   in if xs == [] then library Map.!> buffer
                  else output ++ unzipit'' library' buffer' (tail xs)

