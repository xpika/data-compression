module Main where

import Lzw

main = do
       let toZip = "TOBEORNOTTOBEORTOBEORNOT" 
       print toZip
       let zipped = zipit toZip
       print zipped
       let unzipped = unzipit zipped
       print unzipped
       print "~~~~"
       let toZip = "aaaaaa"
       print toZip
       let zipped = zipit toZip
       print zipped
       let unzipped = unzipit zipped
       print unzipped
       print "~~~~"
       let toZip = "abababab"
       print toZip
       let zipped = zipit toZip
       print zipped
       let unzipped = unzipit zipped
       print unzipped
       let toZip = "abcabcabcabcabcabc"
       print toZip
       let zipped = zipit toZip
       print zipped
       let unzipped = unzipit zipped
       print unzipped
