-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Json
  ( renderJson
  , parseJson
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Char (isDigit, ord)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (showHex)
import Suz.Codec.JsonValue
import Suz.Model
import Text.Read (readMaybe)

-- | JSON text of a document on one line. The document is an object with a
-- @segments@ list. A segment has an @id@ and @tokens@, a token has an @id@, a
-- @surface@ form and @morphs@, a morph has an @id@, a @form@ and a @gloss@.
-- A gloss is @null@ for a morph without one, otherwise an object with the
-- @lexical@ part and the list of @tags@. A morph with a UD annotation also has a
-- @ud@ object with the strings @lemma@, @upos@, @xpos@, @feats@, @deprel@,
-- @deps@ and @misc@ (empty when the column is not given) and a @head@ that is
-- @null@ when not given, @"root"@ for the root and the identifier of another
-- morph otherwise. Identifiers are numbers up to 2^64-1.
renderJson :: Document -> Text
renderJson doc = object [("segments", array (map segment (documentSegments doc)))]

segment :: Segment -> Text
segment s = object [("id", number (let SegmentId n = segmentId s in n)), ("tokens", array (map token (segmentTokens s)))]

token :: Token -> Text
token t = object [("id", number (let TokenId n = tokenId t in n)), ("surface", string (tokenSurface t)), ("morphs", array (map morph (tokenMorphs t)))]

morph :: Morph -> Text
morph m =
  object
    ( [ ("id", number (let MorphId n = morphId m in n))
      , ("form", string (morphForm m))
      , ("gloss", maybe "null" gloss (morphGloss m))
      ]
        ++ [("ud", ud u) | Just u <- [morphUd m]]
    )

ud :: UdFields -> Text
ud u =
  object
    [ ("lemma", string (udLemma u))
    , ("upos", string (udUpos u))
    , ("xpos", string (udXpos u))
    , ("feats", string (udFeats u))
    , ("head", headValue (udHead u))
    , ("deprel", string (udDeprel u))
    , ("deps", string (udDeps u))
    , ("misc", string (udMisc u))
    ]
  where
    headValue NoHead = "null"
    headValue RootHead = string "root"
    headValue (HeadOf (MorphId n)) = number n

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

-- | Reads the form written by 'renderJson'. Other JSON with the same
-- structure is accepted: members may be in any order, white space is
-- allowed, unknown members are ignored, a missing @gloss@ or @ud@ means none
-- and a missing column of @ud@ is empty. Identifiers must be unique. The next
-- free identifier of the document is one more than the largest one, so it can
-- be smaller than it was when the JSON was written. The error holds a
-- description with the line for a syntax error and the path of the member
-- otherwise.
parseJson :: Text -> Either String Document
parseJson input = do
  root <- first (\(n, msg) -> "line " ++ show n ++ ": " ++ msg) (parseJsonValue input)
  members <- asObject "document" root
  items <- field "document" "segments" members >>= asArray "segments"
  segments <- traverse (\(i, v) -> readSegment ("segments[" ++ show i ++ "]") v) (zip [0 :: Int ..] items)
  let ids = concatMap segmentNodeIds segments
  _ <- foldM (\seen n -> if Set.member n seen then Left ("duplicate identifier: " ++ show n) else Right (Set.insert n seen)) Set.empty ids
  next <- case ids of
    [] -> Right 0
    _
      | maximum ids == maxBound -> Left "identifier is too large"
      | otherwise -> Right (maximum ids + 1)
  pure (Document next segments)

type Path = String

readSegment :: Path -> JsonValue -> Either String Segment
readSegment path v = do
  members <- asObject path v
  sid <- field path "id" members >>= asId (path ++ ".id")
  items <- field path "tokens" members >>= asArray (path ++ ".tokens")
  tokens <- traverse (\(i, t) -> readToken (path ++ ".tokens[" ++ show i ++ "]") t) (zip [0 :: Int ..] items)
  pure (Segment (SegmentId sid) tokens)

readToken :: Path -> JsonValue -> Either String Token
readToken path v = do
  members <- asObject path v
  tid <- field path "id" members >>= asId (path ++ ".id")
  surface <- field path "surface" members >>= asString (path ++ ".surface")
  items <- field path "morphs" members >>= asArray (path ++ ".morphs")
  morphs <- traverse (\(i, m) -> readMorph (path ++ ".morphs[" ++ show i ++ "]") m) (zip [0 :: Int ..] items)
  pure (Token (TokenId tid) surface morphs)

readMorph :: Path -> JsonValue -> Either String Morph
readMorph path v = do
  members <- asObject path v
  mid <- field path "id" members >>= asId (path ++ ".id")
  form <- field path "form" members >>= asString (path ++ ".form")
  g <- case lookup "gloss" members of
    Nothing -> Right Nothing
    Just JsonNull -> Right Nothing
    Just x -> Just <$> readGloss (path ++ ".gloss") x
  u <- case lookup "ud" members of
    Nothing -> Right Nothing
    Just JsonNull -> Right Nothing
    Just x -> Just <$> readUd (path ++ ".ud") x
  pure (Morph (MorphId mid) form g u)

readGloss :: Path -> JsonValue -> Either String Gloss
readGloss path v = do
  members <- asObject path v
  lexical <- field path "lexical" members >>= asString (path ++ ".lexical")
  items <- field path "tags" members >>= asArray (path ++ ".tags")
  tags <- traverse (\(i, t) -> asString (path ++ ".tags[" ++ show i ++ "]") t) (zip [0 :: Int ..] items)
  pure (Gloss lexical tags)

readUd :: Path -> JsonValue -> Either String UdFields
readUd path v = do
  members <- asObject path v
  let column name = maybe (Right "") (asString (path ++ "." ++ name)) (lookup (T.pack name) members)
  lemma <- column "lemma"
  upos <- column "upos"
  xpos <- column "xpos"
  feats <- column "feats"
  hd <- case lookup "head" members of
    Nothing -> Right NoHead
    Just JsonNull -> Right NoHead
    Just (JsonString "root") -> Right RootHead
    Just x@(JsonNumber _) -> HeadOf . MorphId <$> asId (path ++ ".head") x
    Just _ -> Left (path ++ ".head: bad head")
  deprel <- column "deprel"
  deps <- column "deps"
  misc <- column "misc"
  pure (UdFields lemma upos xpos feats hd deprel deps misc)

field :: Path -> String -> [(Text, JsonValue)] -> Either String JsonValue
field path name members = maybe (Left (path ++ ": missing member: " ++ name)) Right (lookup (T.pack name) members)

asObject :: Path -> JsonValue -> Either String [(Text, JsonValue)]
asObject _ (JsonObject members) = Right members
asObject path _ = Left (path ++ ": expected an object")

asArray :: Path -> JsonValue -> Either String [JsonValue]
asArray _ (JsonArray items) = Right items
asArray path _ = Left (path ++ ": expected an array")

asString :: Path -> JsonValue -> Either String Text
asString _ (JsonString t) = Right t
asString path _ = Left (path ++ ": expected a string")

asId :: Path -> JsonValue -> Either String Word64
asId _ (JsonNumber t)
  | T.all isDigit t
  , T.length t <= 20
  , Just n <- readMaybe (T.unpack t) :: Maybe Integer
  , n <= fromIntegral (maxBound :: Word64) =
      Right (fromInteger n)
asId path _ = Left (path ++ ": bad identifier")
