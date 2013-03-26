module Main where

import Lzw
import System.Environment
import qualified Data.ByteString (interact) 

main = do
       args <- getArgs
       case args of
         [arg] -> case arg of
             "zip" -> Data.ByteString.interact zipit
             "unzip" -> Data.ByteString.interact unzipit
