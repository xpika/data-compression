module Main where

import Lzw
import Data.ByteString (pack,unpack)
import Data.Char

zipitString str = zipit (pack (map (fromIntegral . ord) str))
unzipitString str = (map chr (map fromIntegral (unpack (unzipit str))))

main = do
       let toZip = "TOBEORNOTTOBEORTOBEORNOT" 
       print toZip
       let zipped = zipitString toZip
       print zipped
       let unzipped = unzipitString zipped
       print unzipped
       print "~~~~"
       let toZip = "aaaaaa"
       print toZip
       let zipped = zipitString toZip
       print zipped
       let unzipped = unzipitString zipped
       print unzipped
       print "~~~~"
       let toZip = "abababab"
       print toZip
       let zipped = zipitString toZip
       print zipped
       let unzipped = unzipitString zipped
       print unzipped
       let toZip = "abcabcabcabcabcabc"
       print toZip
       let zipped = zipitString toZip
       print zipped
       let unzipped = unzipitString zipped
       print unzipped
