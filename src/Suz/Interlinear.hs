-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Interlinear
  ( Names (..)
  , blocks
  , segmentFromLines
  , tokenFrom
  ) where

import Control.Monad (unless, zipWithM)
import Data.List (mapAccumL)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Language
import Suz.Model

-- | How the three lines of a segment are called in error messages.
data Names = Names
  { nameWords :: Text
  , nameMorphs :: Text
  , nameGlosses :: Text
  }

-- | Runs of non-blank lines, each line with its number.
blocks :: [(Int, Text)] -> [[(Int, Text)]]
blocks [] = []
blocks ls = case break blank (dropWhile blank ls) of
  ([], _) -> []
  (g, rest) -> g : blocks rest
  where
    blank = T.null . T.strip . snd

-- | Segment from a line of words and optional lines of morph forms and
-- glosses with their line numbers. Tokens are separated by whitespace, morphs
-- by hyphens, and @_@ stands for a token without morphs. A gloss is split at
-- periods: the first part is a tag when the language lists it as one and the
-- lexical part otherwise, the rest are tags. @?@ is an unglossed morph. The
-- identifiers start at the given number; the next free one is returned.
segmentFromLines :: Language -> Names -> Word64 -> Text -> Maybe (Int, Text) -> Maybe (Int, Text) -> Either (Int, String) (Word64, Segment)
segmentFromLines lang names n tx mb ge = do
  let surfaces = T.words tx
  forms <- case mb of
    Nothing -> Right (map (: []) surfaces)
    Just (ln, c) -> do
      let ws = T.words c
      unless (length ws == length surfaces) (Left (ln, mismatch (nameMorphs names) (nameWords names)))
      pure (map splitCell ws)
  glosses <- case ge of
    Nothing -> Right (map (map (const Nothing)) forms)
    Just (ln, c) -> do
      let ws = T.words c
      unless (length ws == length surfaces) (Left (ln, mismatch (nameGlosses names) (nameWords names)))
      zipWithM (glossCell ln) forms (map splitCell ws)
  let (end, tokens) = mapAccumL tokenFrom (n + 1) (zip3 surfaces forms glosses)
  pure (end, Segment (SegmentId n) tokens)
  where
    mismatch a b = T.unpack (a <> " does not match " <> b)
    splitCell "_" = []
    splitCell w = T.splitOn "-" w
    glossCell ln fs gs = do
      unless (length fs == length gs) (Left (ln, mismatch (nameGlosses names) (nameMorphs names)))
      pure (map (parseGloss lang) gs)

parseGloss :: Language -> Text -> Maybe Gloss
parseGloss _ "?" = Nothing
parseGloss lang p
  | Set.member first (languageTags lang) = Just (Gloss "" (first : later))
  | otherwise = Just (Gloss first later)
  where
    (first, rest) = T.breakOn "." p
    later = if T.null rest then [] else T.splitOn "." (T.drop 1 rest)

-- | Token with consecutive identifiers starting at the given one, and the next
-- free identifier.
tokenFrom :: Word64 -> (Text, [Text], [Maybe Gloss]) -> (Word64, Token)
tokenFrom n (surface, forms, glosses) =
  ( n + 1 + fromIntegral (length forms)
  , Token (TokenId n) surface (zipWith3 (\i f g -> Morph (MorphId i) f g) [n + 1 ..] forms glosses)
  )
