module Main where

import Codec.HaskellCompression
import System.Environment
import qualified Data.ByteString (interact) 

main = do
       args <- getArgs
       case args of
         [arg] -> case arg of
             "zip" -> Data.ByteString.interact zipit
             "unzip" -> Data.ByteString.interact unzipit
             _ -> usage
         _ -> usage

usage = putStrLn "Usage: zip|unzip"
