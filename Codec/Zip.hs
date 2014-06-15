module Codec.Zip where

import Data.List
import qualified Data.Bimap as Map
import Data.Maybe
import Data.BooleanList
import qualified Data.ByteString as B (pack,unpack,ByteString)
import Control.Arrow

viaNum f d = B.pack ( map fromIntegral (f ( map fromIntegral (B.unpack d))))
lengthOfKeys = 127
things xs = (head xs,tail xs)

zipit :: B.ByteString -> B.ByteString
zipit = viaNum zipitNew
zipitNew = zipit'
zipit' xs = (if xs == [] then [] else zipit'' (Map.fromList (zip (Data.List.map ((:[])) [0..lengthOfKeys]) [0..])) headxs tailxs)
  where (headxs,tailxs) = things xs

zipit'' library buffer xs = let 
  (headxs,tailxs) = things xs
  key = [buffer,headxs]
  in if xs == [] then [buffer] else case Map.lookup key library of
     Just n -> zipit'' library n tailxs
     _ -> [buffer] ++ zipit'' (Map.insert key (Map.size library) library) headxs tailxs
