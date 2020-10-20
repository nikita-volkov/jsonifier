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
of encoding typical documents using this library, "aeson" and "buffer-builder".
Every approach is measured on Twitter API data of sizes ranging from roughly 1kB to 60MB.
"aeson" stands for "aeson" producing a strict bytestring,
"lazy-aeson" - lazy bytestring,
"lazy-aeson-untrimmed-32k" - lazy bytestring using an untrimmed builder strategy with allocation of 32k.
"buffer-builder" is another library providing an alternative JSON encoder.

```
jsonifier/1kB                    mean 2.037 μs  ( +- 15.93 ns  )
jsonifier/6kB                    mean 12.68 μs  ( +- 272.7 ns  )
jsonifier/60kB                   mean 122.7 μs  ( +- 3.081 μs  )
jsonifier/600kB                  mean 1.304 ms  ( +- 16.41 μs  )
jsonifier/6MB                    mean 20.98 ms  ( +- 825.8 μs  )
jsonifier/60MB                   mean 197.1 ms  ( +- 14.81 ms  )
aeson/1kB                        mean 6.470 μs  ( +- 118.5 ns  )
aeson/6kB                        mean 31.42 μs  ( +- 680.3 ns  )
aeson/60kB                       mean 265.0 μs  ( +- 5.558 μs  )
aeson/600kB                      mean 3.435 ms  ( +- 99.90 μs  )
aeson/6MB                        mean 30.57 ms  ( +- 470.7 μs  )
aeson/60MB                       mean 278.5 ms  ( +- 6.307 ms  )
lazy-aeson/1kB                   mean 6.419 μs  ( +- 183.5 ns  )
lazy-aeson/6kB                   mean 30.72 μs  ( +- 501.1 ns  )
lazy-aeson/60kB                  mean 257.0 μs  ( +- 4.227 μs  )
lazy-aeson/600kB                 mean 2.533 ms  ( +- 61.61 μs  )
lazy-aeson/6MB                   mean 25.08 ms  ( +- 263.9 μs  )
lazy-aeson/60MB                  mean 249.5 ms  ( +- 1.333 ms  )
lazy-aeson-untrimmed-32k/1kB     mean 6.952 μs  ( +- 427.0 ns  )
lazy-aeson-untrimmed-32k/6kB     mean 29.68 μs  ( +- 656.5 ns  )
lazy-aeson-untrimmed-32k/60kB    mean 259.8 μs  ( +- 4.344 μs  )
lazy-aeson-untrimmed-32k/600kB   mean 2.521 ms  ( +- 21.90 μs  )
lazy-aeson-untrimmed-32k/6MB     mean 25.25 ms  ( +- 295.5 μs  )
lazy-aeson-untrimmed-32k/60MB    mean 250.8 ms  ( +- 3.536 ms  )
buffer-builder/1kB               mean 5.573 μs  ( +- 151.5 ns  )
buffer-builder/6kB               mean 30.40 μs  ( +- 457.2 ns  )
buffer-builder/60kB              mean 308.9 μs  ( +- 4.601 μs  )
buffer-builder/600kB             mean 3.020 ms  ( +- 54.79 μs  )
buffer-builder/6MB               mean 33.55 ms  ( +- 497.8 μs  )
buffer-builder/60MB              mean 316.1 ms  ( +- 3.747 ms  )
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
