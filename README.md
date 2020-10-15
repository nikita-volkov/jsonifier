# Summary

Minimalistic library for encoding JSON directly to strict bytestring.

The library focuses on 2 aspects: **simplicity** and **performance**.
The API consists of just a few functions and
achieves performance that gets up to **3 times** better than that of "aeson"
in typical use-cases.
In cases where we deal with really large documents (60MB) the performance
of "aeson" becomes on par or even supercedes "jsonifier".

# Performance

## Benchmarks

Following are the benchmark results comparing the performance
of encoding typical documents using this library and "aeson".
The numbers after the slash identify the amount of objects in
the rendered JSON.
"lazy-aeson" stands for "aeson" producing a lazy bytestring
(that is what it's optimized for).

```
jsonifier/1                              mean 1.933 μs  ( +- 11.00 ns  )
jsonifier/10                             mean 12.05 μs  ( +- 317.7 ns  )
jsonifier/100                            mean 113.6 μs  ( +- 735.5 ns  )
jsonifier/1,000                          mean 1.262 ms  ( +- 12.04 μs  )
jsonifier/10,000                         mean 27.84 ms  ( +- 478.4 μs  )
jsonifier/100,000                        mean 282.6 ms  ( +- 34.22 ms  )
aeson/1                                  mean 5.334 μs  ( +- 50.73 ns  )
aeson/10                                 mean 26.18 μs  ( +- 192.7 ns  )
aeson/100                                mean 231.1 μs  ( +- 1.856 μs  )
aeson/1,000                              mean 3.523 ms  ( +- 95.93 μs  )
aeson/10,000                             mean 30.04 ms  ( +- 843.5 μs  )
aeson/100,000                            mean 280.4 ms  ( +- 8.853 ms  )
lazy-aeson/1                             mean 5.356 μs  ( +- 50.25 ns  )
lazy-aeson/10                            mean 25.79 μs  ( +- 202.8 ns  )
lazy-aeson/100                           mean 227.4 μs  ( +- 7.047 μs  )
lazy-aeson/1,000                         mean 2.306 ms  ( +- 31.83 μs  )
lazy-aeson/10,000                        mean 22.49 ms  ( +- 285.6 μs  )
lazy-aeson/100,000                       mean 221.3 ms  ( +- 1.212 ms  )
```

Here are the data sizes of the documents by the amounts of objects:

- 1: 941B
- 10: 6.4kB
- 100: 60kB
- 1,000: 604kB
- 10,000: 6MB
- 100,000: 60MB

The benchmark suite is bundled with the package.

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
