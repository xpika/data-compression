module Codec.TestZip where 

import Codec.ExampleStrings 
import Codec.Zip
import Control.Monad
import qualified Data.ByteString.Char8 as B
import Data.Char

main = forM_  examples $ \eg -> do
         print eg
         print $ zipit (B.pack eg)
  
