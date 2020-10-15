# Summary

Minimalistic library for encoding JSON directly to strict bytestring.

The library focuses on 2 aspects: **simplicity** and **performance**.
The API consists of just a few functions and
achieves performance that is **3 times** better than that of "aeson"
in typical use-cases.
No case in which "aeson" performs better has been observed.

# Performance

## Benchmarks

Following are the benchmark results comparing the performance
of encoding typical documents using this library and "aeson".
The numbers after the slash identify the amount of objects in
the rendered JSON.
"lazy-aeson" stands for "aeson" producing a lazy bytestring
(that is what it's optimized for).

```
jsonifier/1          mean 1.970 μs  ( +- 21.85 ns  )
jsonifier/10         mean 12.12 μs  ( +- 259.9 ns  )
jsonifier/100        mean 117.4 μs  ( +- 2.291 μs  )
jsonifier/1000       mean 1.295 ms  ( +- 22.47 μs  )
jsonifier/10000      mean 20.48 ms  ( +- 1.076 ms  )
aeson/1              mean 6.687 μs  ( +- 106.5 ns  )
aeson/10             mean 31.19 μs  ( +- 845.4 ns  )
aeson/100            mean 262.8 μs  ( +- 6.484 μs  )
aeson/1000           mean 2.944 ms  ( +- 207.0 μs  )
aeson/10000          mean 29.94 ms  ( +- 957.9 μs  )
lazy-aeson/1         mean 6.454 μs  ( +- 114.4 ns  )
lazy-aeson/10        mean 30.69 μs  ( +- 773.1 ns  )
lazy-aeson/100       mean 259.2 μs  ( +- 2.485 μs  )
lazy-aeson/1000      mean 2.542 ms  ( +- 60.51 μs  )
lazy-aeson/10000     mean 25.42 ms  ( +- 269.7 μs  )
```

The benchmark suite is bundled with the package, so you can observe
the results yourself.

## Reasoning

Such performance is achieved due to the approach taken to the process of building a bytestring. Unlike "aeson", this library doesn't use the builder distributed with the "bytestring" package, instead it uses a custom solution which produces a bytestring in two steps: first it counts how many bytes the rendering of data will occupy then it allocates a buffer of that exact size and renders directly into it. As the benchmarks show, at least for the purpose of rendering JSON this approach turns out to be faster than manipulations on temporary buffers which the builder from "bytestring" does.

This approach opens doors to optimizations otherwise inaccessible. E.g., we can efficiently count how many bytes a `Text` value encoded as JSON string literal will occupy, then render it into its final destination in one pass. We can efficiently count how many bytes a decimal encoding of an integer will occupy, and also render it in one pass despite the rendering of integers having to be done in reverse direction and requiring a second pass of reversing the bytes in alternative solutions.

*With all those observations some general concepts have emerged and have been extracted as the lower-level ["ptr-poker" package](https://github.com/nikita-volkov/ptr-poker), which focuses on the problem of populating pointers.*

# Quality

The quality of the library is ensured with a test property in which a random JSON tree is generated, then rendered using "jsonifier", then parsed using "aeson" and compared to the original.

# Demo

Following is a complete program that shows how you can render
JSON from your domain model.

```haskell
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import qualified Jsonifier as J
import qualified Data.ByteString.Char8 as Char8ByteString


{-|
Outputs the following:

> {"name":"Metallica","genres":[{"name":"Metal"},{"name":"Rock"},{"name":"Blues"}]}
-}
main =
  Char8ByteString.putStrLn (J.toByteString (artistJson metallica))

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
