{-# LANGUAGE OverloadedStrings #-}

import qualified JsonLego as J
import qualified Data.ByteString.Char8 as AsciiByteString


-- |
-- Outputs the following:
-- 
-- > {"name":"Metallica","genres":[{"name":"Metal"},{"name":"Rock"},{"name":"Blues"}]}
main =
  AsciiByteString.putStrLn (J.json (artistJson metallica))

metallica :: Artist
metallica =
  Artist "Metallica" [Genre "Metal", Genre "Rock", Genre "Blues"]


-- * Model
-------------------------

data Artist =
  Artist { artistName :: Text, artistGenres :: [Genre] }

data Genre =
  Genre { genreName :: Text }


-- * Builders
-------------------------

artistJson :: Artist -> J.Json
artistJson (Artist name genres) =
  J.object [
    ("name", J.string name),
    ("genres", J.array (fmap genreJson genres))
    ]

genreJson :: Genre -> J.Json
genreJson (Genre name) =
  J.object [
    ("name", J.string name)
    ]
