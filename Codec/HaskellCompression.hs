module Codec.HaskellCompression where 

import qualified Codec.HaskellCompression.Zip
import qualified Codec.HaskellCompression.Unzip

zipit =  Codec.HaskellCompression.Zip.zipit
unzipit =  Codec.HaskellCompression.Unzip.unzipit
