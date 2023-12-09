# Summary

Minimalistic library for encoding JSON directly to strict bytestring.

The library focuses on 2 aspects: **simplicity** and **performance**.
The API consists of just a few functions and
achieves performance that gets up to **3 times** better than that of "aeson"
in typical use-cases.
In cases where we deal with very large documents the performance difference
becomes less drastic.

# Performance

## Benchmarks

Following are the benchmark results comparing the performance
of encoding typical documents using this library and "aeson".
Every approach is measured on Twitter API data of sizes ranging from roughly 1kB to 60MB.
"aeson" stands for "aeson" producing a strict bytestring,
"lazy-aeson" - lazy bytestring,
"lazy-aeson-untrimmed-32k" - lazy bytestring using an untrimmed builder strategy with allocation of 32k.

```
1kB/jsonifier                            mean 2.054 μs  ( +- 30.83 ns  )
1kB/aeson                                mean 6.456 μs  ( +- 126.7 ns  )
1kB/lazy-aeson                           mean 6.338 μs  ( +- 169.1 ns  )
1kB/lazy-aeson-untrimmed-32k             mean 6.905 μs  ( +- 280.2 ns  )

6kB/jsonifier                            mean 12.80 μs  ( +- 196.9 ns  )
6kB/aeson                                mean 31.28 μs  ( +- 733.2 ns  )
6kB/lazy-aeson                           mean 30.30 μs  ( +- 229.5 ns  )
6kB/lazy-aeson-untrimmed-32k             mean 29.17 μs  ( +- 371.3 ns  )

60kB/jsonifier                           mean 122.9 μs  ( +- 1.492 μs  )
60kB/aeson                               mean 258.4 μs  ( +- 1.000 μs  )
60kB/lazy-aeson                          mean 259.4 μs  ( +- 4.494 μs  )
60kB/lazy-aeson-untrimmed-32k            mean 255.7 μs  ( +- 3.239 μs  )

600kB/jsonifier                          mean 1.299 ms  ( +- 16.44 μs  )
600kB/aeson                              mean 3.389 ms  ( +- 106.8 μs  )
600kB/lazy-aeson                         mean 2.520 ms  ( +- 45.51 μs  )
600kB/lazy-aeson-untrimmed-32k           mean 2.509 ms  ( +- 30.76 μs  )

6MB/jsonifier                            mean 20.91 ms  ( +- 821.7 μs  )
6MB/aeson                                mean 30.74 ms  ( +- 509.4 μs  )
6MB/lazy-aeson                           mean 24.83 ms  ( +- 184.3 μs  )
6MB/lazy-aeson-untrimmed-32k             mean 24.93 ms  ( +- 383.2 μs  )

60MB/jsonifier                           mean 194.8 ms  ( +- 13.93 ms  )
60MB/aeson                               mean 276.0 ms  ( +- 5.194 ms  )
60MB/lazy-aeson                          mean 246.9 ms  ( +- 3.122 ms  )
60MB/lazy-aeson-untrimmed-32k            mean 245.1 ms  ( +- 1.050 ms  )
```

The benchmark suite is bundled with the package.

## Reasoning

Such performance is achieved due to the approach taken to the process of building a bytestring. Unlike "aeson", this library doesn't use the builder distributed with the "bytestring" package, instead it uses a custom solution which produces a bytestring in two steps: first it counts how many bytes the rendering of data will occupy then it allocates a buffer of that exact size and renders directly into it. As the benchmarks show, at least for the purpose of rendering JSON this approach turns out to be faster than manipulations on temporary buffers which the builder from "bytestring" does.

This approach opens doors to optimizations otherwise inaccessible. E.g., we can efficiently count how many bytes a `Text` value encoded as JSON string literal will occupy, then render it into its final destination in one pass. We can efficiently count how many bytes a decimal encoding of an integer will occupy, and also render it in one pass despite the rendering of integers needing to be done in reverse direction and requiring a second pass of reversing the bytes in alternative solutions.

*With all those observations some general concepts have emerged and have been extracted as the lower-level ["ptr-poker" package](https://github.com/nikita-volkov/ptr-poker), which focuses on the problem of populating pointers.*

# Quality

The quality of the library is ensured with a test property in which a random JSON tree is generated, then rendered using "jsonifier", then parsed using "aeson" and compared to the original.

# Demo

Following is a complete program that shows how you can render
JSON from your domain model.

```haskell
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import qualified Jsonifier as J
import qualified Data.ByteString.Char8


{-|
Outputs the following:

> {"name":"Metallica","genres":[{"name":"Metal"},{"name":"Rock"},{"name":"Blues"}]}
-}
main =
  Data.ByteString.Char8.putStrLn (J.toByteString (artistJson metallica))

metallica :: Artist
metallica =
  Artist "Metallica" [Genre "Metal", Genre "Rock", Genre "Blues"]


-- * Model
-------------------------

data Artist =
  Artist { artistName :: Text, artistGenres :: [Genre] }

data Genre =
  Genre { genreName :: Text }


-- * Encoders
-------------------------

artistJson :: Artist -> J.Json
artistJson Artist{..} =
  J.object [
    ("name", J.textString artistName),
    ("genres", J.array (fmap genreJson artistGenres))
    ]

genreJson :: Genre -> J.Json
genreJson Genre{..} =
  J.object [
    ("name", J.textString genreName)
    ]
```

A compilable version of this demo comes bundled with the package as the \"demo\" test-suite.
