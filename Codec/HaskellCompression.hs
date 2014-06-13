module Codec.HaskellCompression 
where 
import Data.List
import qualified Data.Bimap as Map
import Data.Maybe

import qualified Data.ByteString as B (pack,unpack,ByteString)


viaNum f d = B.pack ( map fromIntegral (f (map fromIntegral (B.unpack d))))

lengthOfKeys = 127

zipit :: B.ByteString -> B.ByteString
zipit = viaNum zipit'
zipit' [] = []
zipit' (x:xs) = zipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) x xs
zipit'' library buffer [] = buffer : []
zipit'' library buffer (x:xs) = let key = [buffer,x]
                                    ( output
                                     ,buffer'
                                     ,library'
                                     ) = case Map.lookup key library of
                                               Just n -> (  id 
                                                           ,n 
                                                           ,library
                                                         )
                                               _ -> (  (:) buffer
                                                      ,x
                                                      ,Map.insert key (Map.size library) library 
                                                    )
                                in output $ zipit'' library' buffer' xs



unzipit :: B.ByteString-> B.ByteString                            
unzipit = viaNum unzipit'
unzipit' [] = []
unzipit' (x:xs) = unzipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) x xs
unzipit'' library buffer [] =  library Map.!> buffer
unzipit'' library buffer xs = let 
   headxs = head xs
   Just key = buffer `Map.lookupR` library 
   librarySize = Map.size library 
   ref = fromJust (if headxs == librarySize then Just key else headxs `Map.lookupR` library)
   ( output ,buffer',library') = case Map.lookup [buffer,headxs] library of
      Just n -> ( [] ,n ,library)
      _ -> (key, headxs ,Map.insert (key ++ ref) librarySize library )
   in output ++ unzipit'' library' buffer' (tail xs)
