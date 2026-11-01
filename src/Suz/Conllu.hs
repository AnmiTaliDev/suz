-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Conllu
  ( parseConllu
  , parseConlluWith
  , renderConllu
  ) where

import Data.Bifunctor (first)
import Data.Char (isDigit, isSpace)
import Data.List (mapAccumL)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Interlinear
import Suz.Language
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
-- description. A @Gloss=@ item of MISC is not read as a gloss, see
-- 'parseConlluWith'.
parseConllu :: Text -> Either (Int, String) Document
parseConllu = readConllu Nothing

-- | Like 'parseConllu', and a @Gloss=@ item in the MISC column of a word line
-- is the gloss of its morph, as 'renderConllu' writes it. The item is left out
-- of the MISC text of the UD annotation. The value is split at periods: the
-- first part is a tag when the language lists it as one and the lexical part
-- otherwise, the rest are tags. @%25@, @%7C@, @%20@, @%09@, @%0A@ and @%0D@ in
-- the value stand for a percent sign, a vertical bar, a space, a tab and the
-- line breaks, other text after a percent sign is kept. The first such item of
-- a line is used, an empty value leaves the morph unglossed.
parseConlluWith :: Language -> Text -> Either (Int, String) Document
parseConlluWith = readConllu . Just

readConllu :: Maybe Language -> Text -> Either (Int, String) Document
readConllu lang text = do
  sentences <- traverse (traverse row) (blocks content)
  let kept = filter (not . null . tokensOf . map pair) sentences
      (next, built) = mapAccumL segment 0 kept
  segments <- traverse (uncurry (annotate lang)) built
  pure (Document next segments)
  where
    content = [(n, l) | (n, l) <- zip [1 ..] (T.lines text), not ("#" `T.isPrefixOf` l)]
    pair r = (rowIdent r, rowForm r)
    segment n rows =
      let (end, ts) = mapAccumL tokenFrom (n + 1) (tokensOf (map pair rows))
       in (end, (Segment (SegmentId n) ts, rows))

annotate :: Maybe Language -> Segment -> [Row] -> Either (Int, String) Segment
annotate lang seg rows = do
  uds <- traverse (udOf lang ids) wordRows
  let annotated = zipWith (\m (u, g) -> m {morphUd = u, morphGloss = g}) morphs uds
      (_, tokens) = mapAccumL give annotated (segmentTokens seg)
  pure seg {segmentTokens = tokens}
  where
    wordRows = [(k, r) | r <- rows, Word k <- [rowIdent r]]
    morphs = concatMap tokenMorphs (segmentTokens seg)
    ids = Map.fromList (zip (map fst wordRows) (map morphId morphs))
    give ms t = let (mine, rest) = splitAt (length (tokenMorphs t)) ms in (rest, t {tokenMorphs = mine})

udOf :: Maybe Language -> Map.Map Int MorphId -> (Int, Row) -> Either (Int, String) (Maybe UdFields, Maybe Gloss)
udOf lang ids (_, r) = do
  hd <- headOf (rowCols r !! 4)
  (misc, gloss) <- case lang of
    Nothing -> Right (col 7, Nothing)
    Just l -> glossOf l (col 7)
  let ud = UdFields (col 0) (col 1) (col 2) (col 3) hd (col 5) (col 6) misc
  pure (if ud == noUd then Nothing else Just ud, gloss)
  where
    glossOf l misc = do
      let items = T.splitOn "|" misc
          isGloss = ("Gloss=" `T.isPrefixOf`)
          rest = T.intercalate "|" (filter (not . isGloss) items)
          value = T.drop (T.length "Gloss=") <$> filter isGloss items
      gloss <- case value of
        v : _ | not (T.null v) -> first (\(_, msg) -> (rowLine r, msg)) (parseGloss l False (\_ t -> Right t) 0 (unescapeMisc v))
        _ -> Right Nothing
      pure (rest, gloss)
    col i = let t = rowCols r !! i in if t == "_" then "" else t
    noUd = UdFields "" "" "" "" NoHead "" "" ""
    headOf "_" = Right NoHead
    headOf "0" = Right RootHead
    headOf t
      | not (T.null t) && T.all isDigit t, Just m <- Map.lookup (read (T.unpack t)) ids = Right (HeadOf m)
      | otherwise = Left (rowLine r, "bad HEAD")

-- | CoNLL-U text of a document, readable by 'parseConllu'. A token with
-- several morphs is written as a range line with the surface form followed by
-- a line per morph with the morph form; any other token is one line with the
-- surface form. The UD annotation of a morph fills the LEMMA, UPOS, XPOS,
-- FEATS, HEAD, DEPREL, DEPS and MISC columns of its line, the columns of a
-- range line and of a morph without annotation hold @_@. A head in another
-- sentence is written as @_@ and DEPS is written as it is stored. Segments
-- without tokens are skipped. Whitespace in a form is written as a no-break
-- space, an empty form as @_@, and a tab or line break in another column as a
-- space. The gloss of a morph is written as a @Gloss=@ item in its MISC
-- column, after the other items of the UD annotation and replacing a
-- @Gloss=@ item there, as its lexical part and tags joined by periods with a
-- percent sign, a vertical bar, a space, a tab and the line breaks written as
-- @%25@, @%7C@, @%20@, @%09@, @%0A@ and @%0D@. Identifiers and the morph forms
-- of single-morph tokens are not kept.
renderConllu :: Document -> Text
renderConllu doc = T.unlines (concatMap sentence (filter (not . null . segmentTokens) (documentSegments doc)))
  where
    sentence s = concatMap (uncurry (tokenRows (locals s))) (numbered s) ++ [""]
    numbered s = snd (mapAccumL number 1 (segmentTokens s))
    number i t = (i + max 1 (length (tokenMorphs t)), (t, i))
    locals s = Map.fromList [(morphId m, k) | (t, i) <- numbered s, (k, m) <- zip [i ..] (tokenMorphs t)]
    tokenRows ls t i = case tokenMorphs t of
      ms@(_ : _ : _) ->
        let end = i + length ms - 1
         in line (T.pack (show i ++ "-" ++ show end)) (tokenSurface t) (replicate 8 "_") : [line (T.pack (show k)) (morphForm m) (udColumns ls m) | (k, m) <- zip [i ..] ms]
      [m] -> [line (T.pack (show i)) (tokenSurface t) (udColumns ls m)]
      [] -> [line (T.pack (show i)) (tokenSurface t) (replicate 8 "_")]
    line ident form cols = T.intercalate "\t" (ident : formCell form : cols)
    formCell form
      | T.null form = "_"
      | otherwise = T.map (\c -> if isSpace c then '\xa0' else c) form
    udColumns ls m = case morphUd m of
      Nothing -> replicate 7 "_" ++ [col (miscOf m "")]
      Just u -> [col (udLemma u), col (udUpos u), col (udXpos u), col (udFeats u), headCol ls (udHead u), col (udDeprel u), col (udDeps u), col (miscOf m (udMisc u))]
    miscOf m misc = case morphGloss m of
      Nothing -> misc
      Just g -> T.intercalate "|" (filter (not . T.null) (filter (not . ("Gloss=" `T.isPrefixOf`)) (T.splitOn "|" misc) ++ ["Gloss=" <> escapeMisc (glossText g)]))
    glossText (Gloss lexical tags) = T.intercalate "." ([lexical | not (T.null lexical)] ++ tags)
    col t
      | T.null t = "_"
      | otherwise = T.map (\c -> if c `elem` ("\t\n\r" :: String) then ' ' else c) t
    headCol _ NoHead = "_"
    headCol _ RootHead = "0"
    headCol ls (HeadOf m) = maybe "_" (T.pack . show) (Map.lookup m ls)

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

escapeMisc :: Text -> Text
escapeMisc = T.concatMap one
  where
    one '%' = "%25"
    one '|' = "%7C"
    one ' ' = "%20"
    one '\t' = "%09"
    one '\n' = "%0A"
    one '\r' = "%0D"
    one c = T.singleton c

unescapeMisc :: Text -> Text
unescapeMisc t = case T.breakOn "%" t of
  (before, "") -> before
  (before, rest) -> case lookup (T.take 3 rest) table of
    Just c -> before <> T.cons c (unescapeMisc (T.drop 3 rest))
    Nothing -> before <> "%" <> unescapeMisc (T.drop 1 rest)
  where
    table = [("%25", '%'), ("%7C", '|'), ("%20", ' '), ("%09", '\t'), ("%0A", '\n'), ("%0D", '\r')]
