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
--zipit' :: [[Char]] -> Char -> [Char] -> [Char]
zipit'' library buffer [] = buffer : []
zipit'' library buffer (x:xs) = translated $ zipit'' updatedLibrary updateBuffer xs
  where (translated,updateBuffer,updatedLibrary) = case Map.lookup [buffer,x] library of
                                                     Just n -> (id,n,library)
                                                     _ -> ( (:) buffer
                                                           ,x
                                                           ,Map.insert ([buffer,x]) (Map.size library) library 
                                                          )

unzipit :: (String -> String)
unzipit = viaNum unzipit'
unzipit' [] = []
unzipit' (x:xs) = unzipit'' (Map.fromList (zip (Data.List.map ((:[]) ) [0..lengthOfKeys]) [0..])) x xs
unzipit'' library buffer [] = ( (Map.!>) library (buffer)) ++ []
unzipit'' library buffer (x:xs) = translated $ unzipit'' updatedLibrary updateBuffer xs
  where (translated,updateBuffer,updatedLibrary) = case Map.lookup [buffer,x] (library) of
                                                     Just n -> (id,n,library)
                                                     _ -> ( (++) ( library Map.!> buffer),x,Map.insert (( library Map.!> buffer)++(take 1 $ library Map.!> x )) (Map.size library) library )
