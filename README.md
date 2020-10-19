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
The numbers after the slash identify the amount of objects in
the rendered JSON.
"lazy-aeson" stands for "aeson" producing a lazy bytestring,
otherwise it's strict.
"buffer-builder" is another library providing an alternative JSON encoder.

```
jsonifier/1              mean 2.038 μs  ( +- 14.23 ns  )
jsonifier/10             mean 12.55 μs  ( +- 178.1 ns  )
jsonifier/100            mean 120.8 μs  ( +- 2.406 μs  )
jsonifier/1,000          mean 1.287 ms  ( +- 19.42 μs  )
jsonifier/10,000         mean 20.70 ms  ( +- 964.8 μs  )
jsonifier/100,000        mean 195.1 ms  ( +- 14.70 ms  )
aeson/1                  mean 6.412 μs  ( +- 65.73 ns  )
aeson/10                 mean 31.20 μs  ( +- 831.4 ns  )
aeson/100                mean 261.0 μs  ( +- 3.612 μs  )
aeson/1,000              mean 3.379 ms  ( +- 93.11 μs  )
aeson/10,000             mean 30.58 ms  ( +- 847.8 μs  )
aeson/100,000            mean 278.3 ms  ( +- 5.669 ms  )
lazy-aeson/1             mean 6.390 μs  ( +- 49.62 ns  )
lazy-aeson/10            mean 30.24 μs  ( +- 245.1 ns  )
lazy-aeson/100           mean 259.0 μs  ( +- 2.995 μs  )
lazy-aeson/1,000         mean 2.538 ms  ( +- 47.43 μs  )
lazy-aeson/10,000        mean 24.86 ms  ( +- 153.9 μs  )
lazy-aeson/100,000       mean 247.7 ms  ( +- 594.4 μs  )
buffer-builder/1         mean 5.473 μs  ( +- 90.16 ns  )
buffer-builder/10        mean 29.72 μs  ( +- 531.4 ns  )
buffer-builder/100       mean 304.6 μs  ( +- 6.911 μs  )
buffer-builder/1,000     mean 3.006 ms  ( +- 73.80 μs  )
buffer-builder/10,000    mean 33.19 ms  ( +- 480.9 μs  )
buffer-builder/100,000   mean 310.3 ms  ( +- 2.987 ms  )
```

Here is the listing of the data sizes of produced documents by the amounts of objects:

- 1: 941B
- 10: 6.4kB
- 100: 60kB
- 1,000: 604kB
- 10,000: 6MB
- 100,000: 60MB

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
