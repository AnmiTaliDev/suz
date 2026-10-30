-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Toolbox
  ( renderToolbox
  , parseToolbox
  ) where

import Control.Monad (foldM)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Interlinear
import Suz.Language
import Suz.Model

-- | Toolbox (SFM) text of a document. Each segment is a record of four
-- lines, @\\ref@ with the segment identifier, @\\tx@ with the surface forms,
-- @\\mb@ with the morph forms and @\\ge@ with the glosses, and records are
-- separated by a blank line. Tokens are separated by spaces and morphs by
-- hyphens. A morph without a gloss shows as @?@. A token without morphs shows
-- as @_@ in @\\mb@ and @\\ge@. A backslash, hyphen or period in a form, lexical
-- part or tag is written after a backslash, any whitespace character as
-- @\\u@, its code in hexadecimal and a semicolon, and a lone @_@ or @?@ after a
-- backslash. An empty form or an empty gloss cannot be written and does not
-- survive 'parseToolbox'.
renderToolbox :: Document -> Text
renderToolbox = T.intercalate "\n" . map record . documentSegments

-- | Reads a document from Toolbox text. Records are separated by blank lines;
-- a record without a @\\tx@ line, such as a file header, is skipped, and
-- markers other than @\\tx@, @\\mb@ and @\\ge@ are ignored. Without @\\mb@ each
-- token gets one morph with the surface form as its form, and without @\\ge@
-- the morphs are unglossed. A gloss is split at periods: the first part is a
-- tag when the language lists it as one and the lexical part otherwise, the
-- rest are tags. The escapes of 'renderToolbox' are read; a bad one is an
-- error. Identifiers are assigned in document order from 0. The error
-- holds the line number and a short description.
parseToolbox :: Language -> Text -> Either (Int, String) Document
parseToolbox lang text = do
  records <- traverse fieldsOf (blocks (zip [1 ..] (T.lines text)))
  (next, segments) <- foldM step (0, []) records
  pure (Document next (reverse segments))
  where
    step (n, acc) fields = case lookupField "tx" fields of
      Nothing -> Right (n, acc)
      Just tx -> do
        (n', seg) <- segmentFromLines lang names n tx (lookupField "mb" fields) (lookupField "ge" fields)
        pure (n', seg : acc)

names :: Names
names = Names "\\tx" "\\mb" "\\ge" True

record :: Segment -> Text
record (Segment (SegmentId n) tokens) =
  T.unlines
    [ "\\ref " <> T.pack (show n)
    , "\\tx " <> T.unwords (map (escapeCell . tokenSurface) tokens)
    , "\\mb " <> T.unwords (map (cell (escapeCell . morphForm)) tokens)
    , "\\ge " <> T.unwords (map (cell (glossPiece . morphGloss)) tokens)
    ]
  where
    cell f t = case tokenMorphs t of
      [] -> "_"
      ms -> case T.intercalate "-" (map f ms) of
        "_" -> "\\_"
        c -> c
    glossPiece Nothing = "?"
    glossPiece (Just (Gloss lexical tags)) = case T.intercalate "." (filter (not . T.null) [escapeCell lexical] ++ map escapeCell tags) of
      "?" -> "\\?"
      c -> c

type Field = (Int, Text, Text)

fieldsOf :: [(Int, Text)] -> Either (Int, String) [Field]
fieldsOf = traverse field
  where
    field (n, l) = case T.uncons l of
      Just ('\\', rest) ->
        let (marker, content) = T.break isSpace rest
         in Right (n, marker, T.strip content)
      _ -> Left (n, "line out of place")

lookupField :: Text -> [Field] -> Maybe (Int, Text)
lookupField marker fields = case [(n, c) | (n, m, c) <- fields, m == marker] of
  f : _ -> Just f
  [] -> Nothing
