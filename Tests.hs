module Tests where

import Lzw

main = do
       let toZip =  "TOBEORNOTTOBEORTOBEORNOT" 
       print toZip
       let zipped = zipit toZip
       print zipped
       let unzipped = unzipit zipped
       print unzipped
       let toZip =  "aaa" 
       print toZip
       let zipped = zipit toZip
       print zipped
       let unzipped = unzipit zipped
       print unzipped
	
