module Main where

import Lzw
import qualified Data.ByteString as B
import Data.Char

zipitString str = zipit (B.pack (map (fromIntegral . ord) str))
unzipitString str = (map chr (map fromIntegral (B.unpack (unzipit str))))

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
