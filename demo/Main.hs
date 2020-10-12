{-# LANGUAGE OverloadedStrings #-}

import qualified JsonLego as J
import qualified Data.ByteString.Char8 as AsciiByteString


-- |
-- Outputs the following:
-- 
-- > {"name":"Metallica","genres":[{"name":"Metal"},{"name":"Rock"},{"name":"Blues"}]}
main =
  AsciiByteString.putStrLn (J.value metallica)


-- * Model
-------------------------

data Artist =
  Artist { artistName :: Text, artistGenres :: [Genre] }

data Genre =
  Genre { genreName :: Text }


-- * Builders
-------------------------

artist :: Artist -> J.Value
artist (Artist name genres) =
  J.object [
    ("name", J.string name),
    ("genres", J.array (fmap genre genres))
    ]

genre :: Genre -> J.Value
genre (Genre name) =
  J.object [
    ("name", J.string name)
    ]

metallica :: J.Value
metallica =
  artist (Artist "Metallica" [Genre "Metal", Genre "Rock", Genre "Blues"])
