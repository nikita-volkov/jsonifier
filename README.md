# Summary

Minimalistic library for encoding JSON directly to strict bytestring.

The library focuses on 2 aspects: **simplicity** and **performance**.
The API consists of just a few functions and
achieves performance that gets up to **3 times** better than that of "aeson"
in typical use-cases.
In cases where we deal with really large documents (60MB) the performance
of "aeson" becomes more comparable.

# Performance

## Benchmarks

Following are the benchmark results comparing the performance
of encoding typical documents using this library, "aeson" and "buffer-builder".
Every approach is measured on Twitter API data of sizes ranging from roughly 1kB to 60MB.
"lazy-aeson" stands for "aeson" producing a lazy bytestring,
otherwise it's strict.
"buffer-builder" is another library providing an alternative JSON encoder.

```
jsonifier/1kB          mean 2.087 μs  ( +- 260.0 ns  )
jsonifier/6kB          mean 12.33 μs  ( +- 222.2 ns  )
jsonifier/60kB         mean 118.3 μs  ( +- 1.991 μs  )
jsonifier/600kB        mean 1.270 ms  ( +- 38.92 μs  )
jsonifier/6MB          mean 20.53 ms  ( +- 1.042 ms  )
jsonifier/60MB         mean 194.9 ms  ( +- 15.04 ms  )
aeson/1kB              mean 6.542 μs  ( +- 199.2 ns  )
aeson/6kB              mean 31.25 μs  ( +- 494.5 ns  )
aeson/60kB             mean 261.7 μs  ( +- 8.044 μs  )
aeson/600kB            mean 3.395 ms  ( +- 114.6 μs  )
aeson/6MB              mean 30.71 ms  ( +- 701.0 μs  )
aeson/60MB             mean 277.1 ms  ( +- 4.776 ms  )
lazy-aeson/1kB         mean 6.423 μs  ( +- 83.69 ns  )
lazy-aeson/6kB         mean 30.74 μs  ( +- 607.0 ns  )
lazy-aeson/60kB        mean 259.1 μs  ( +- 4.890 μs  )
lazy-aeson/600kB       mean 2.511 ms  ( +- 18.71 μs  )
lazy-aeson/6MB         mean 24.92 ms  ( +- 95.36 μs  )
lazy-aeson/60MB        mean 248.6 ms  ( +- 736.6 μs  )
buffer-builder/1kB     mean 5.512 μs  ( +- 77.39 ns  )
buffer-builder/6kB     mean 30.29 μs  ( +- 459.9 ns  )
buffer-builder/60kB    mean 307.0 μs  ( +- 3.640 μs  )
buffer-builder/600kB   mean 3.001 ms  ( +- 75.72 μs  )
buffer-builder/6MB     mean 33.05 ms  ( +- 336.3 μs  )
buffer-builder/60MB    mean 308.5 ms  ( +- 3.489 ms  )
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
