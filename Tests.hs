module Main where

import Codec.HaskellCompression
import qualified Data.ByteString.Char8 as B
import Codec.HaskellCompression.ExampleStrings 

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
       mapM_ example examples

