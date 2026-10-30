-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Import
  ( importText
  , importTextWith
  ) where

import Data.List (mapAccumL)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Language
import Suz.Model

-- | Document from raw text. Every line with at least one word becomes a
-- segment, every whitespace-separated word becomes a token, and every token
-- gets one unglossed morph whose form is the word. Identifiers are consecutive
-- from 0 in document order. Punctuation stays attached to its word.
importText :: Text -> Document
importText = importTextWith (mkLanguage "" [] [])

-- | Like 'importText', with the import rules of the language, see
-- 'withImportRules'. Each punctuation character at the start or end of a word
-- becomes a token of its own, in text order. The form of a morph is lowercased
-- as the language asks; the surface form keeps its case.
importTextWith :: Language -> Text -> Document
importTextWith lang text = Document next segments
  where
    ls = filter (not . null) (map (concatMap pieces . T.words) (T.lines text))
    (next, segments) = mapAccumL (segmentFrom lang) 0 ls
    isPunctuation = (`Set.member` languagePunctuation lang)
    pieces w = map T.singleton (T.unpack lead) ++ [core | not (T.null core)] ++ map T.singleton (T.unpack trail)
      where
        (lead, rest) = T.span isPunctuation w
        core = T.dropWhileEnd isPunctuation rest
        trail = T.drop (T.length core) rest

segmentFrom :: Language -> Word64 -> [Text] -> (Word64, Segment)
segmentFrom lang n ws = (end, Segment (SegmentId n) tokens)
  where
    (end, tokens) = mapAccumL (tokenFrom lang) (n + 1) ws

tokenFrom :: Language -> Word64 -> Text -> (Word64, Token)
tokenFrom lang n w = (n + 2, Token (TokenId n) w [Morph (MorphId (n + 1)) form Nothing])
  where
    form = foldCase (languageCaseFolding lang) w

foldCase :: CaseFolding -> Text -> Text
foldCase NoFolding = id
foldCase SimpleFolding = T.toLower
foldCase TurkicFolding = T.concatMap fold
  where
    fold 'I' = "\x131"
    fold '\x130' = "i"
    fold c = T.toLower (T.singleton c)
