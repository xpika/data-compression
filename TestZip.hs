module Codec.TestZip where 

import Codec.HaskellCompression.Zip
import Codec.HaskellCompression.Unzip
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char

main = forM_  ["a","aa","the rain in spain falls mainly on the plain.","tobeornottobeortobeornot"] $ \eg -> do
         print eg
         let a = zipit (B.pack eg)
         let b = B.unpack $ unzipit a
         print a
         print b
