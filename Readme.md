haskell-compression
=======

```haskell
Codec.HaskellCompression Data.ByteString.Lazy.Char8> let str = "The rain in spain falls mainly on the plain"
*Codec.HaskellCompression Data.ByteString.Lazy.Char8> Prelude.length str
43
*Codec.HaskellCompression Data.ByteString.Lazy.Char8> let zipped = zipit (pack str)
*Codec.HaskellCompression Data.ByteString.Lazy.Char8> Data.ByteString.Lazy.Char8.length zipped
39
*Codec.HaskellCompression Data.ByteString.Lazy.Char8> unzipit zipped
"The rain in spain falls mainly on the plain"
```
