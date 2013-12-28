module Lzw where 
import Data.List
import qualified Data.Bimap as Map

import Data.ByteString (pack,unpack,ByteString)

viaNum f d = pack ( map fromIntegral (f (map fromIntegral (unpack d))))

lengthOfKeys = 127

zipit :: ByteString-> ByteString
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



unzipit :: ByteString-> ByteString                            
unzipit = viaNum unzipit'
unzipit' [] = []
unzipit' (x:xs) = unzipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) x xs
unzipit'' library buffer [] =  library Map.!> buffer
unzipit'' library buffer (x:xs) = let Just key = buffer `Map.lookupR` library 
                                      librarySize = Map.size library 
                                      (  output
                                        ,buffer'
                                        ,library'
                                       ) = case Map.lookup [buffer,x] library of
                                                     Just n -> ( id
                                                                ,n
                                                                ,library
                                                                )
                                                     _ -> (  (++) key
                                                            , x
                                                            , let Just key' | x == librarySize = Just key
                                                                            | otherwise =  x `Map.lookupR` library  
                                                              in Map.insert (key ++ (take 1 key')) librarySize library 
                                                           )
                                  in output $ unzipit'' library' buffer' xs
