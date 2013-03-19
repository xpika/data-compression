module Main where

import Lzw
import System.Environment

main = do
       args <- getArgs
       case args of
         [arg] -> case arg of
             "zip" -> interact zipit
             "unzip" -> interact unzipit
