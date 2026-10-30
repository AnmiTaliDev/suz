-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Conllu
  ( parseConllu
  , renderConllu
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (mapAccumL)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Interlinear
import Suz.Model

data Ident = Word Int | Range Int Int | Empty

-- | Reads a document from CoNLL-U text. Every sentence becomes a segment and
-- every word line becomes a token with one unglossed morph whose form is the
-- FORM column. A multiword token range such as @1-2@ becomes one token with
-- the range FORM as surface form and one unglossed morph per covered word.
-- Empty nodes, comment lines and all columns except ID and FORM are ignored.
-- Identifiers are assigned in document order from 0. The error holds the
-- line number and a short description.
parseConllu :: Text -> Either (Int, String) Document
parseConllu text = do
  sentences <- traverse (traverse row) (blocks content)
  let (next, segments) = mapAccumL segment 0 (filter (not . null) (map tokensOf sentences))
  pure (Document next segments)
  where
    content = [(n, l) | (n, l) <- zip [1 ..] (T.lines text), not ("#" `T.isPrefixOf` l)]
    segment n tokens =
      let (end, ts) = mapAccumL tokenFrom (n + 1) tokens
       in (end, Segment (SegmentId n) ts)

-- | CoNLL-U text of a document, readable by 'parseConllu'. Only the ID and
-- FORM columns are filled, the others hold @_@. A token with several morphs is
-- written as a range line with the surface form followed by a line per morph
-- form; any other token is one line with the surface form. Segments without
-- tokens are skipped. Whitespace in a form is written as a no-break space and
-- an empty form as @_@. Identifiers and the morph forms of single-morph tokens
-- are not kept.
renderConllu :: Document -> Text
renderConllu doc = T.unlines (concatMap sentence (filter (not . null . segmentTokens) (documentSegments doc)))
  where
    sentence s = concat (snd (mapAccumL token 1 (segmentTokens s))) ++ [""]
    token i t = case tokenMorphs t of
      ms | length ms > 1 ->
        let end = i + length ms - 1
         in (end + 1, line (T.pack (show i ++ "-" ++ show end)) (tokenSurface t) : [line (T.pack (show k)) (morphForm m) | (k, m) <- zip [i ..] ms])
      _ -> (i + 1, [line (T.pack (show i)) (tokenSurface t)])
    line ident form = T.intercalate "\t" (ident : cell form : replicate 8 "_")
    cell form
      | T.null form = "_"
      | otherwise = T.map (\c -> if isSpace c then '\xa0' else c) form

row :: (Int, Text) -> Either (Int, String) (Ident, Text)
row (n, l) = case T.splitOn "\t" l of
  cols@(i : form : _) | length cols == 10 -> (\ident -> (ident, form)) <$> identOf n i
  _ -> Left (n, "expected 10 columns")

identOf :: Int -> Text -> Either (Int, String) Ident
identOf n i
  | number i = Right (Word (read (T.unpack i)))
  | [a, b] <- T.splitOn "-" i, number a, number b = Right (Range (read (T.unpack a)) (read (T.unpack b)))
  | [a, b] <- T.splitOn "." i, number a, number b = Right Empty
  | otherwise = Left (n, "bad ID")
  where
    number t = not (T.null t) && T.all isDigit t

tokensOf :: [(Ident, Text)] -> [(Text, [Text], [Maybe a])]
tokensOf [] = []
tokensOf ((Empty, _) : rest) = tokensOf rest
tokensOf ((Word _, form) : rest) = (form, [form], [Nothing]) : tokensOf rest
tokensOf ((Range a b, form) : rest) = (form, forms, map (const Nothing) forms) : tokensOf after
  where
    (inside, after) = span (covered a b . fst) rest
    forms = map snd inside
    covered lo hi (Word k) = lo <= k && k <= hi
    covered _ _ _ = False
