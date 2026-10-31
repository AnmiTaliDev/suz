-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Json
  ( renderJson
  ) where

import Data.Char (ord)
import Data.Text (Text)
import qualified Data.Text as T
import Numeric (showHex)
import Suz.Model

-- | JSON text of a document on one line. The document is an object with a
-- @segments@ list. A segment has an @id@ and @tokens@, a token has an @id@, a
-- @surface@ form and @morphs@, a morph has an @id@, a @form@ and a @gloss@.
-- A gloss is @null@ for a morph without one, otherwise an object with the
-- @lexical@ part and the list of @tags@. Identifiers are numbers up to 2^64-1.
renderJson :: Document -> Text
renderJson doc = object [("segments", array (map segment (documentSegments doc)))]

segment :: Segment -> Text
segment s = object [("id", number (let SegmentId n = segmentId s in n)), ("tokens", array (map token (segmentTokens s)))]

token :: Token -> Text
token t = object [("id", number (let TokenId n = tokenId t in n)), ("surface", string (tokenSurface t)), ("morphs", array (map morph (tokenMorphs t)))]

morph :: Morph -> Text
morph m =
  object
    [ ("id", number (let MorphId n = morphId m in n))
    , ("form", string (morphForm m))
    , ("gloss", maybe "null" gloss (morphGloss m))
    ]

gloss :: Gloss -> Text
gloss g = object [("lexical", string (glossLexical g)), ("tags", array (map string (glossTags g)))]

object :: [(Text, Text)] -> Text
object fields = "{" <> T.intercalate "," [string k <> ":" <> v | (k, v) <- fields] <> "}"

array :: [Text] -> Text
array items = "[" <> T.intercalate "," items <> "]"

number :: Show a => a -> Text
number = T.pack . show

string :: Text -> Text
string t = "\"" <> T.concatMap escape t <> "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
      | c < ' ' = "\\u" <> T.justifyRight 4 '0' (T.pack (showHex (ord c) ""))
      | otherwise = T.singleton c
