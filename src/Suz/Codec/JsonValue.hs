-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec.JsonValue
  ( JsonValue (..)
  , parseJsonValue
  ) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T

-- | A JSON value. A number is kept as the text that was written.
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Text
  | JsonString Text
  | JsonArray [JsonValue]
  | JsonObject [(Text, JsonValue)]
  deriving (Eq, Show)

-- A parser takes the rest of the input. A failure holds the rest of the input
-- at the point of the error and a description.
type Parser a = Text -> Either (Text, String) (a, Text)

maxDepth :: Int
maxDepth = 100

-- | Reads one JSON value (RFC 8259) with nothing after it but white space. The
-- error holds the line number and a short description.
parseJsonValue :: Text -> Either (Int, String) JsonValue
parseJsonValue input = case value 0 input of
  Left (rest, msg) -> Left (lineAt rest, msg)
  Right (v, rest)
    | T.null (skipSpace rest) -> Right v
    | otherwise -> Left (lineAt (skipSpace rest), "unexpected text after the value")
  where
    lineAt rest = 1 + T.count "\n" (T.take (T.length input - T.length rest) input)

skipSpace :: Text -> Text
skipSpace = T.dropWhile (`elem` (" \t\n\r" :: String))

value :: Int -> Parser JsonValue
value depth t0
  | depth > maxDepth = Left (t0, "nesting is too deep")
  | otherwise = case T.uncons t of
      Nothing -> Left (t, "unexpected end of text")
      Just ('{', r) -> object (depth + 1) (skipSpace r)
      Just ('[', r) -> array (depth + 1) (skipSpace r)
      Just ('"', r) -> do
        (s, rest) <- string r
        pure (JsonString s, rest)
      Just (c, _)
        | c == '-' || isDigit c -> number t
      _
        | Just r <- T.stripPrefix "null" t -> Right (JsonNull, r)
        | Just r <- T.stripPrefix "true" t -> Right (JsonBool True, r)
        | Just r <- T.stripPrefix "false" t -> Right (JsonBool False, r)
        | otherwise -> Left (t, "unexpected character")
  where
    t = skipSpace t0

number :: Parser JsonValue
number t
  | validNumber (T.unpack word) = Right (JsonNumber word, rest)
  | otherwise = Left (t, "bad number")
  where
    (word, rest) = T.span (`elem` ("+-.eE0123456789" :: String)) t

validNumber :: String -> Bool
validNumber s0 = case integer (dropMinus s0) of
  Nothing -> False
  Just s1 -> maybe False exponent' (fraction s1)
  where
    dropMinus ('-' : r) = r
    dropMinus r = r
    integer ('0' : r) = Just r
    integer r@(c : _) | isDigit c && c /= '0' = Just (dropWhile isDigit r)
    integer _ = Nothing
    fraction ('.' : r) = case span isDigit r of
      ([], _) -> Nothing
      (_, r') -> Just r'
    fraction r = Just r
    exponent' (e : r) | e `elem` ("eE" :: String) = case r of
      c : r' | c `elem` ("+-" :: String) -> digits r'
      _ -> digits r
    exponent' r = null r
    digits r = case span isDigit r of
      ([], _) -> False
      (_, r') -> null r'

string :: Parser Text
string = go []
  where
    go acc t =
      let (chunk, rest) = T.break (\c -> c == '"' || c == '\\' || c < ' ') t
          acc' = chunk : acc
       in case T.uncons rest of
            Nothing -> Left (rest, "unterminated string")
            Just ('"', r) -> Right (T.concat (reverse acc'), r)
            Just ('\\', r) -> escape acc' r
            Just _ -> Left (rest, "control character in string")
    escape acc t = case T.uncons t of
      Nothing -> Left (t, "unterminated string")
      Just (c, r) -> case c of
        '"' -> go (T.singleton '"' : acc) r
        '\\' -> go (T.singleton '\\' : acc) r
        '/' -> go (T.singleton '/' : acc) r
        'b' -> go (T.singleton '\b' : acc) r
        'f' -> go (T.singleton '\f' : acc) r
        'n' -> go (T.singleton '\n' : acc) r
        'r' -> go (T.singleton '\r' : acc) r
        't' -> go (T.singleton '\t' : acc) r
        'u' -> unicode acc r
        _ -> Left (t, "bad escape")
    unicode acc t = do
      (hi, r) <- hex4 t
      if hi >= 0xD800 && hi <= 0xDBFF
        then case T.stripPrefix "\\u" r of
          Just r' -> do
            (lo, r'') <- hex4 r'
            if lo >= 0xDC00 && lo <= 0xDFFF
              then go (T.singleton (chr (0x10000 + (hi - 0xD800) * 0x400 + (lo - 0xDC00))) : acc) r''
              else Left (r, "bad surrogate pair")
          Nothing -> Left (r, "bad surrogate pair")
        else
          if hi >= 0xDC00 && hi <= 0xDFFF
            then Left (t, "bad surrogate pair")
            else go (T.singleton (chr hi) : acc) r
    hex4 t =
      let (digits, r) = T.splitAt 4 t
       in if T.length digits == 4 && T.all isHexDigit digits
            then Right (T.foldl' (\n c -> n * 16 + digitToInt c) 0 digits, r)
            else Left (t, "bad unicode escape")

array :: Int -> Parser JsonValue
array depth t = case T.uncons t of
  Just (']', r) -> Right (JsonArray [], r)
  _ -> items [] t
  where
    items acc u = do
      (v, r) <- value depth u
      case T.uncons (skipSpace r) of
        Just (',', r') -> items (v : acc) r'
        Just (']', r') -> Right (JsonArray (reverse (v : acc)), r')
        _ -> Left (skipSpace r, "expected a comma or a closing bracket")

object :: Int -> Parser JsonValue
object depth t = case T.uncons t of
  Just ('}', r) -> Right (JsonObject [], r)
  _ -> members [] t
  where
    members acc u = case T.uncons (skipSpace u) of
      Just ('"', r) -> do
        (key, r1) <- string r
        r2 <- case T.uncons (skipSpace r1) of
          Just (':', r') -> Right r'
          _ -> Left (skipSpace r1, "expected a colon")
        (v, r3) <- value depth r2
        case T.uncons (skipSpace r3) of
          Just (',', r') -> members ((key, v) : acc) r'
          Just ('}', r') -> Right (JsonObject (reverse ((key, v) : acc)), r')
          _ -> Left (skipSpace r3, "expected a comma or a closing brace")
      _ -> Left (skipSpace u, "expected a member name")

