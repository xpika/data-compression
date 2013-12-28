module Main where

import Codec.HaskellCompression
import qualified Data.ByteString.Char8 as B

zipitString :: [Char] -> B.ByteString
zipitString  = zipit  . B.pack

unzipitString :: B.ByteString -> [Char]
unzipitString  = B.unpack . unzipit

example str = do 
              print str
              let zipped = zipitString str
              print zipped 
              let unzipped = unzipitString zipped
              print unzipped
              putStrLn ("~~~~ "++show (B.length zipped)++"/"++ (show $ length unzipped))
            
main = do
       example "TOBEORNOTTOBEORTOBEORNOT" 
       example "aaaaaa"
       example "abababab"
       example "abcabcabcabcabcabc"
       example "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nam dictum nisi et ipsum ornare fermentum. Pellentesque habitant morbi tristique senectus et netus et malesuada fames ac turpis egestas nullam. "
