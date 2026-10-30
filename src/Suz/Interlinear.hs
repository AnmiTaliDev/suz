-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Interlinear
  ( Names (..)
  , blocks
  , segmentFromLines
  , tokenFrom
  , escapeCell
  ) where

import Control.Monad (unless, zipWithM)
import Data.Bifunctor (first)
import Data.Char (chr, isHexDigit, isSpace, ord)
import Data.List (mapAccumL)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Numeric (readHex, showHex)
import Suz.Language
import Suz.Model

-- | How the three lines of a segment are called in error messages, and
-- whether their cells use backslash escapes, see 'escapeCell'.
data Names = Names
  { nameWords :: Text
  , nameMorphs :: Text
  , nameGlosses :: Text
  , nameEscaped :: Bool
  }

-- | Runs of non-blank lines, each line with its number.
blocks :: [(Int, Text)] -> [[(Int, Text)]]
blocks [] = []
blocks ls = case break blank (dropWhile blank ls) of
  ([], _) -> []
  (g, rest) -> g : blocks rest
  where
    blank = T.null . T.strip . snd

-- | Text of a cell with the characters that carry meaning replaced by
-- escapes: a backslash, a hyphen and a period by a backslash and the
-- character, any whitespace character by @\\u@, its code in hexadecimal and a
-- semicolon.
escapeCell :: Text -> Text
escapeCell = T.concatMap escapeChar
  where
    escapeChar c
      | c `elem` ("\\-." :: String) = T.pack ['\\', c]
      | isSpace c = T.pack ("\\u" ++ showHex (ord c) ";")
      | otherwise = T.singleton c

unescapeCell :: Text -> Either String Text
unescapeCell = fmap T.pack . go . T.unpack
  where
    go [] = Right []
    go ('\\' : 'u' : xs) = case break (== ';') xs of
      (hex, ';' : rest)
        | not (null hex), all isHexDigit hex, [(n, "")] <- readHex hex, valid n -> (chr n :) <$> go rest
      _ -> Left "bad escape"
    go ('\\' : x : xs)
      | x `elem` ("\\-.?_" :: String) = (x :) <$> go xs
    go ('\\' : _) = Left "bad escape"
    go (x : xs) = (x :) <$> go xs
    valid n = n <= 0x10FFFF && not (n >= 0xD800 && n <= 0xDFFF)

splitCell :: Bool -> Char -> Text -> [Text]
splitCell False c = T.split (== c)
splitCell True c = map T.pack . go [] . T.unpack
  where
    go acc [] = [reverse acc]
    go acc ('\\' : x : xs) = go (x : '\\' : acc) xs
    go acc (x : xs)
      | x == c = reverse acc : go [] xs
      | otherwise = go (x : acc) xs

-- | Segment from a line of words and optional lines of morph forms and
-- glosses, each with its line number. Tokens are separated by whitespace,
-- morphs by hyphens, and @_@ stands for a token without morphs. A gloss is
-- split at periods: the first part is a tag when the language lists it as one
-- and the lexical part otherwise, the rest are tags. @?@ is an unglossed
-- morph. The identifiers start at the given number; the next free one is
-- returned.
segmentFromLines :: Language -> Names -> Word64 -> (Int, Text) -> Maybe (Int, Text) -> Maybe (Int, Text) -> Either (Int, String) (Word64, Segment)
segmentFromLines lang names n (txLn, tx) mb ge = do
  surfaces <- traverse (plain txLn) (T.words tx)
  forms <- case mb of
    Nothing -> Right (map (: []) surfaces)
    Just (ln, c) -> do
      let ws = T.words c
      unless (length ws == length surfaces) (Left (ln, mismatch (nameMorphs names) (nameWords names)))
      traverse (traverse (plain ln) . pieces) ws
  glosses <- case ge of
    Nothing -> Right (map (map (const Nothing)) forms)
    Just (ln, c) -> do
      let ws = T.words c
      unless (length ws == length surfaces) (Left (ln, mismatch (nameGlosses names) (nameWords names)))
      zipWithM (glossCell ln) forms (map pieces ws)
  let (end, tokens) = mapAccumL tokenFrom (n + 1) (zip3 surfaces forms glosses)
  pure (end, Segment (SegmentId n) tokens)
  where
    escaped = nameEscaped names
    mismatch a b = T.unpack (a <> " does not match " <> b)
    pieces "_" = []
    pieces w = splitCell escaped '-' w
    plain ln t
      | escaped = first (const (ln, "bad escape")) (unescapeCell t)
      | otherwise = Right t
    glossCell ln fs gs = do
      unless (length fs == length gs) (Left (ln, mismatch (nameGlosses names) (nameMorphs names)))
      traverse (parseGloss lang escaped plain ln) gs

parseGloss :: Language -> Bool -> (Int -> Text -> Either (Int, String) Text) -> Int -> Text -> Either (Int, String) (Maybe Gloss)
parseGloss _ _ _ _ "?" = Right Nothing
parseGloss lang escaped plain ln p = do
  parts <- traverse (plain ln) (splitCell escaped '.' p)
  pure $ case parts of
    first' : later
      | Set.member first' (languageTags lang) -> Just (Gloss "" (first' : later))
      | otherwise -> Just (Gloss first' later)
    [] -> Nothing

-- | Token with consecutive identifiers starting at the given one, and the next
-- free identifier.
tokenFrom :: Word64 -> (Text, [Text], [Maybe Gloss]) -> (Word64, Token)
tokenFrom n (surface, forms, glosses) =
  ( n + 1 + fromIntegral (length forms)
  , Token (TokenId n) surface (zipWith3 (\i f g -> Morph (MorphId i) f g) [n + 1 ..] forms glosses)
  )
