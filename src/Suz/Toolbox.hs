-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Toolbox
  ( renderToolbox
  , parseToolbox
  ) where

import Control.Monad (foldM, unless, zipWithM)
import Data.Char (isSpace)
import Data.List (mapAccumL)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Language
import Suz.Model
import Suz.Render (renderGloss)

-- | Toolbox (SFM) text of a document. Each segment is a record of four
-- lines, @\\ref@ with the segment identifier, @\\tx@ with the surface forms,
-- @\\mb@ with the morph forms and @\\ge@ with the glosses, and records are
-- separated by a blank line. Tokens are separated by spaces and morphs by
-- hyphens. A morph without a gloss shows as @?@. A token without morphs shows
-- as @_@ in @\\mb@ and @\\ge@. The text is not escaped, so surface forms with
-- whitespace, morph forms with hyphens, and glosses with periods or hyphens in
-- the lexical part do not survive 'parseToolbox'.
renderToolbox :: Document -> Text
renderToolbox = T.intercalate "\n" . map record . documentSegments

-- | Reads a document from Toolbox text. Records are separated by blank lines;
-- a record without a @\\tx@ line, such as a file header, is skipped, and
-- markers other than @\\tx@, @\\mb@ and @\\ge@ are ignored. Without @\\mb@ each
-- token gets one morph with the surface form as its form, and without @\\ge@
-- the morphs are unglossed. A gloss is split at periods: the first part is a
-- tag when the language lists it as one and the lexical part otherwise, the
-- rest are tags. Identifiers are assigned in document order from 0. The error
-- holds the line number and a short description.
parseToolbox :: Language -> Text -> Either (Int, String) Document
parseToolbox lang text = do
  records <- traverse fieldsOf (groups (zip [1 ..] (T.lines text)))
  (next, segments) <- foldM step (0, []) records
  pure (Document next (reverse segments))
  where
    step (n, acc) fields = case lookupField "tx" fields of
      Nothing -> Right (n, acc)
      Just (_, tx) -> do
        (n', seg) <- segmentFrom lang n tx fields
        pure (n', seg : acc)

record :: Segment -> Text
record (Segment (SegmentId n) tokens) =
  T.unlines
    [ "\\ref " <> T.pack (show n)
    , "\\tx " <> T.unwords (map tokenSurface tokens)
    , "\\mb " <> T.unwords (map (cell morphForm) tokens)
    , "\\ge " <> T.unwords (map (cell (maybe "?" renderGloss . morphGloss)) tokens)
    ]
  where
    cell f t = case tokenMorphs t of
      [] -> "_"
      ms -> T.intercalate "-" (map f ms)

type Field = (Int, Text, Text)

groups :: [(Int, Text)] -> [[(Int, Text)]]
groups [] = []
groups ls = case break blank (dropWhile blank ls) of
  ([], _) -> []
  (g, rest) -> g : groups rest
  where
    blank = T.null . T.strip . snd

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

segmentFrom :: Language -> Word64 -> Text -> [Field] -> Either (Int, String) (Word64, Segment)
segmentFrom lang n tx fields = do
  let surfaces = T.words tx
  forms <- case lookupField "mb" fields of
    Nothing -> Right (map (: []) surfaces)
    Just (ln, c) -> do
      let ws = T.words c
      unless (length ws == length surfaces) (Left (ln, "\\mb does not match \\tx"))
      pure (map splitCell ws)
  glosses <- case lookupField "ge" fields of
    Nothing -> Right (map (map (const Nothing)) forms)
    Just (ln, c) -> do
      let ws = T.words c
      unless (length ws == length surfaces) (Left (ln, "\\ge does not match \\tx"))
      zipWithM (glossCell ln) forms (map splitCell ws)
  let (end, tokens) = mapAccumL tokenFrom (n + 1) (zip3 surfaces forms glosses)
  pure (end, Segment (SegmentId n) tokens)
  where
    splitCell "_" = []
    splitCell w = T.splitOn "-" w
    glossCell ln fs gs = do
      unless (length fs == length gs) (Left (ln, "\\ge does not match \\mb"))
      pure (map (parseGloss lang) gs)

parseGloss :: Language -> Text -> Maybe Gloss
parseGloss _ "?" = Nothing
parseGloss lang p
  | Set.member first (languageTags lang) = Just (Gloss "" (first : later))
  | otherwise = Just (Gloss first later)
  where
    (first, rest) = T.breakOn "." p
    later = if T.null rest then [] else T.splitOn "." (T.drop 1 rest)

tokenFrom :: Word64 -> (Text, [Text], [Maybe Gloss]) -> (Word64, Token)
tokenFrom n (surface, forms, glosses) =
  ( n + 1 + fromIntegral (length forms)
  , Token (TokenId n) surface (zipWith3 (\i f g -> Morph (MorphId i) f g) [n + 1 ..] forms glosses)
  )
