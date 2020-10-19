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
of encoding typical documents using this library and "aeson".
The numbers after the slash identify the amount of objects in
the rendered JSON.
"lazy-aeson" stands for "aeson" producing a lazy bytestring,
otherwise it's strict.

```
jsonifier/1          mean 2.043 μs  ( +- 24.33 ns  )
jsonifier/10         mean 12.60 μs  ( +- 139.3 ns  )
jsonifier/100        mean 120.8 μs  ( +- 3.271 μs  )
jsonifier/1,000      mean 1.275 ms  ( +- 13.45 μs  )
jsonifier/10,000     mean 20.71 ms  ( +- 864.1 μs  )
jsonifier/100,000    mean 195.2 ms  ( +- 15.26 ms  )
aeson/1              mean 6.400 μs  ( +- 40.03 ns  )
aeson/10             mean 31.40 μs  ( +- 760.1 ns  )
aeson/100            mean 262.1 μs  ( +- 4.486 μs  )
aeson/1,000          mean 3.413 ms  ( +- 83.81 μs  )
aeson/10,000         mean 30.38 ms  ( +- 439.9 μs  )
aeson/100,000        mean 275.8 ms  ( +- 5.646 ms  )
lazy-aeson/1         mean 6.403 μs  ( +- 58.98 ns  )
lazy-aeson/10        mean 30.30 μs  ( +- 447.1 ns  )
lazy-aeson/100       mean 257.7 μs  ( +- 4.806 μs  )
lazy-aeson/1,000     mean 2.485 ms  ( +- 24.07 μs  )
lazy-aeson/10,000    mean 24.89 ms  ( +- 447.1 μs  )
lazy-aeson/100,000   mean 245.4 ms  ( +- 1.571 ms  )
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
