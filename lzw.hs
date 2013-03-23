module Lzw where
import Debug.Trace
import Data.List 
import Data.Char
import qualified Data.Bimap as Map
import Data.Maybe
import Control.Monad
import Control.Arrow

viaNum f d = map chr (f (map ord d))

lengthOfKeys = 127 

zipit :: (String -> String)
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


unzipit :: (String -> String)
unzipit = viaNum unzipit'
unzipit' [] = []
unzipit' (x:xs) = unzipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) x xs
unzipit'' library buffer [] =  library Map.!> buffer
unzipit'' library buffer (x:xs) = let key = library Map.!> buffer
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
                                                            , Map.insert (key ++ library Map.!> x) (Map.size library) library 
                                                           )
                                  in output $ unzipit'' library' buffer' xs
