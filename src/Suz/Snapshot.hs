-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Snapshot
  ( renderSnapshot
  , parseSnapshot
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Codec.Escape (decodeLexical, encodeLexical, escape, unescape)
import Suz.Model
import Text.Read (readMaybe)

data Line
  = LineDocument Word64
  | LineSegment SegmentId
  | LineToken TokenId Text
  | LineMorph Morph

-- | Text form of a document: a @document@ line with the next free
-- identifier, then @segment@, @token@ and @morph@ lines in document order,
-- tab-separated. A morph line has the identifier and the form, and for a
-- glossed morph also the lexical part and the tags of the gloss. The undo
-- history is not part of a snapshot.
renderSnapshot :: Document -> Text
renderSnapshot doc =
  T.unlines $
    ["document\t" <> word (documentNextId doc)]
      ++ concatMap segmentLines (documentSegments doc)

-- | Reads a document from the text form of 'renderSnapshot'. Blank lines and
-- lines starting with @#@ are skipped. The error holds the line number (0 when
-- the problem is not tied to a line) and a short description. A snapshot is
-- rejected when it has no @document@ line first, when a line is out of place,
-- when an identifier repeats, or when an identifier is not below the next free
-- identifier.
parseSnapshot :: Text -> Either (Int, String) Document
parseSnapshot text = do
  parsed <- traverse parseLine [(n, l) | (n, l) <- zip [1 ..] (T.lines text), not (skipped l)]
  case parsed of
    (_, LineDocument nextId) : rest -> do
      checkIds nextId rest
      segments <- segmentsP rest
      pure (Document nextId segments)
    _ -> Left (0, "missing document line")

segmentLines :: Segment -> [Text]
segmentLines (Segment (SegmentId n) tokens) = ("segment\t" <> word n) : concatMap tokenLines tokens

tokenLines :: Token -> [Text]
tokenLines (Token (TokenId n) surface morphs) =
  T.intercalate "\t" ["token", word n, escape surface] : map morphLine morphs

morphLine :: Morph -> Text
morphLine (Morph (MorphId n) form gloss _) =
  T.intercalate "\t" (["morph", word n, escape form] ++ glossFields gloss)
  where
    glossFields Nothing = []
    glossFields (Just (Gloss lexical tags)) = encodeLexical lexical : map escape tags

word :: Word64 -> Text
word = T.pack . show

skipped :: Text -> Bool
skipped l = T.null (T.strip l) || "#" `T.isPrefixOf` l

parseLine :: (Int, Text) -> Either (Int, String) (Int, Line)
parseLine (n, raw) = either (Left . (,) n) (Right . (,) n) (decodeLine raw)

decodeLine :: Text -> Either String Line
decodeLine raw = case T.splitOn "\t" raw of
  ["document", nextId] -> LineDocument <$> readId nextId
  ["segment", i] -> LineSegment . SegmentId <$> readId i
  ["token", i, surface] -> LineToken . TokenId <$> readId i <*> unescape surface
  ["morph", i, form] -> do
    mid <- readId i
    f <- unescape form
    pure (LineMorph (Morph (MorphId mid) f Nothing Nothing))
  "morph" : i : form : lexical : tags -> do
    mid <- readId i
    f <- unescape form
    l <- decodeLexical lexical
    ts <- traverse unescape tags
    pure (LineMorph (Morph (MorphId mid) f (Just (Gloss l ts)) Nothing))
  _ -> Left "unrecognized line"

readId :: Text -> Either String Word64
readId t = case readMaybe (T.unpack t) :: Maybe Integer of
  Just n | n >= 0 && n <= fromIntegral (maxBound :: Word64) -> Right (fromIntegral n)
  _ -> Left ("bad number: " ++ T.unpack t)

lineId :: Line -> Maybe Word64
lineId (LineDocument _) = Nothing
lineId (LineSegment (SegmentId n)) = Just n
lineId (LineToken (TokenId n) _) = Just n
lineId (LineMorph (Morph (MorphId n) _ _ _)) = Just n

checkIds :: Word64 -> [(Int, Line)] -> Either (Int, String) ()
checkIds nextId = fmap (const ()) . foldM step Set.empty
  where
    step seen (n, line) = case lineId line of
      Nothing -> Right seen
      Just i
        | i >= nextId -> Left (n, "identifier " ++ show i ++ " is not below the next free identifier")
        | Set.member i seen -> Left (n, "duplicate identifier " ++ show i)
        | otherwise -> Right (Set.insert i seen)

segmentsP :: [(Int, Line)] -> Either (Int, String) [Segment]
segmentsP [] = Right []
segmentsP ((_, LineSegment sid) : rest) = do
  (tokens, rest') <- tokensP rest
  (Segment sid tokens :) <$> segmentsP rest'
segmentsP ((n, _) : _) = Left (n, "line out of place")

tokensP :: [(Int, Line)] -> Either (Int, String) ([Token], [(Int, Line)])
tokensP ((_, LineToken tid surface) : rest) = do
  let (morphs, rest') = morphsP rest
  first (Token tid surface morphs :) <$> tokensP rest'
tokensP rest = Right ([], rest)

morphsP :: [(Int, Line)] -> ([Morph], [(Int, Line)])
morphsP ((_, LineMorph m) : rest) = first (m :) (morphsP rest)
morphsP rest = ([], rest)
