module Codec.HaskellCompression where 

import qualified Codec.Zip
import qualified Codec.Unzip

zipit = Codec.Zip.zipit
unzipit = Codec.Unzip.unzipit
