-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Conllu
  ( parseConllu
  , renderConllu
  ) where

import Data.Char (isDigit, isSpace)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Interlinear
import Suz.Model

data Ident = Word Int | Range Int Int | Empty

-- | Reads a document from CoNLL-U text. Every sentence becomes a segment and
-- every word line becomes a token with one unglossed morph whose form is the
-- FORM column. A multiword token range such as @1-2@ becomes one token with
-- the range FORM as surface form and one unglossed morph per covered word. The
-- LEMMA, UPOS, XPOS, FEATS, HEAD, DEPREL, DEPS and MISC columns of a word line
-- become the UD annotation of its morph, unless all of them are @_@. HEAD must
-- be 0 or the ID of a word of the same sentence. Empty nodes, comment lines
-- and the columns of range lines are ignored. Identifiers are assigned in
-- document order from 0. The error holds the line number and a short
-- description.
parseConllu :: Text -> Either (Int, String) Document
parseConllu text = do
  sentences <- traverse (traverse row) (blocks content)
  let kept = filter (not . null . tokensOf . map pair) sentences
      (next, built) = mapAccumL segment 0 kept
  segments <- traverse (uncurry annotate) built
  pure (Document next segments)
  where
    content = [(n, l) | (n, l) <- zip [1 ..] (T.lines text), not ("#" `T.isPrefixOf` l)]
    pair r = (rowIdent r, rowForm r)
    segment n rows =
      let (end, ts) = mapAccumL tokenFrom (n + 1) (tokensOf (map pair rows))
       in (end, (Segment (SegmentId n) ts, rows))

annotate :: Segment -> [Row] -> Either (Int, String) Segment
annotate seg rows = do
  uds <- traverse (udOf ids) wordRows
  let annotated = zipWith (\m u -> m {morphUd = u}) morphs uds
      (_, tokens) = mapAccumL give annotated (segmentTokens seg)
  pure seg {segmentTokens = tokens}
  where
    wordRows = [(k, r) | r <- rows, Word k <- [rowIdent r]]
    morphs = concatMap tokenMorphs (segmentTokens seg)
    ids = Map.fromList (zip (map fst wordRows) (map morphId morphs))
    give ms t = let (mine, rest) = splitAt (length (tokenMorphs t)) ms in (rest, t {tokenMorphs = mine})

udOf :: Map.Map Int MorphId -> (Int, Row) -> Either (Int, String) (Maybe UdFields)
udOf ids (_, r) = do
  hd <- headOf (rowCols r !! 4)
  let ud = UdFields (col 0) (col 1) (col 2) (col 3) hd (col 5) (col 6) (col 7)
  pure (if ud == noUd then Nothing else Just ud)
  where
    col i = let t = rowCols r !! i in if t == "_" then "" else t
    noUd = UdFields "" "" "" "" NoHead "" "" ""
    headOf "_" = Right NoHead
    headOf "0" = Right RootHead
    headOf t
      | not (T.null t) && T.all isDigit t, Just m <- Map.lookup (read (T.unpack t)) ids = Right (HeadOf m)
      | otherwise = Left (rowLine r, "bad HEAD")

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

data Row = Row
  { rowLine :: Int
  , rowIdent :: Ident
  , rowForm :: Text
  , rowCols :: [Text]
  }

row :: (Int, Text) -> Either (Int, String) Row
row (n, l) = case T.splitOn "\t" l of
  cols@(i : form : rest) | length cols == 10 -> (\ident -> Row n ident form rest) <$> identOf n i
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
