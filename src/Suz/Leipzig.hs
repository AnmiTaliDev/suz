-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Leipzig
  ( parseLeipzig
  ) where

import Control.Monad (foldM)
import qualified Data.Text as T
import Data.Text (Text)
import Suz.Interlinear
import Suz.Language
import Suz.Model

-- | Reads a document from interlinear text as written by 'renderDocument'
-- and by the Leipzig Glossing Rules. Blocks are separated by blank lines and
-- hold a line of words, a line of morph forms and a line of glosses; a fourth
-- line, the free translation, is ignored. Words are separated by whitespace,
-- morphs by hyphens, and @_@ stands for a word without morphs. A gloss is
-- split at periods: the first part is a tag when the language lists it as one
-- and the lexical part otherwise, the rest are tags. @?@ is an unglossed
-- morph. Identifiers are assigned in document order from 0. The error holds
-- the line number and a short description.
parseLeipzig :: Language -> Text -> Either (Int, String) Document
parseLeipzig lang text = do
  (next, segments) <- foldM step (0, []) (blocks (zip [1 ..] (T.lines text)))
  pure (Document next (reverse segments))
  where
    step (n, acc) block = case block of
      [(_, tx), mb, ge] -> add n acc tx mb ge
      [(_, tx), mb, ge, _] -> add n acc tx mb ge
      (ln, _) : _ -> Left (ln, "expected 3 or 4 lines")
      [] -> Right (n, acc)
    add n acc tx mb ge = do
      (n', seg) <- segmentFromLines lang names n tx (Just mb) (Just ge)
      pure (n', seg : acc)
    names = Names "word line" "morph line" "gloss line"
