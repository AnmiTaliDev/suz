-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Import
  ( importText
  ) where

import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Model

-- | Document from raw text. Every line with at least one word becomes a
-- segment, every whitespace-separated word becomes a token, and every token
-- gets one unglossed morph whose form is the word. Identifiers are consecutive
-- from 0 in document order. Punctuation stays attached to its word.
importText :: Text -> Document
importText text = Document next segments
  where
    ls = filter (not . null) (map T.words (T.lines text))
    (next, segments) = mapAccumL segmentFrom 0 ls

segmentFrom :: Word64 -> [Text] -> (Word64, Segment)
segmentFrom n ws = (end, Segment (SegmentId n) tokens)
  where
    (end, tokens) = mapAccumL tokenFrom (n + 1) ws

tokenFrom :: Word64 -> Text -> (Word64, Token)
tokenFrom n w = (n + 2, Token (TokenId n) w [Morph (MorphId (n + 1)) w Nothing])
