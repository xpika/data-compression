haskell-compression
=======

```haskell
*Codec.HaskellCompression Data.ByteString.Char8> zipit (pack "The quick brown fox jumps over the lazy dog")
"The quick brown fox jumps over t\129 lazy dog"
*Codec.HaskellCompression Data.ByteString.Char8> unzipit it
"The quick brown fox jumps over the lazy dog"
```