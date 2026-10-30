-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec
  ( encodeCommand
  , decodeCommand
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Codec.Escape (escape, unescape)
import Suz.Codec.Parser
import Suz.Command
import Suz.Model

-- | One-line text form of a command: tab-separated fields, with backslash,
-- tab, newline and carriage return escaped inside fields.
encodeCommand :: Command -> Text
encodeCommand = T.intercalate "\t" . map escape . commandFields

-- | Inverse of 'encodeCommand'. The error is a short description of the first
-- problem found.
decodeCommand :: Text -> Either String Command
decodeCommand line = do
  fields <- traverse unescape (T.splitOn "\t" line)
  (command, rest) <- runParser commandP fields
  if null rest then Right command else Left "trailing fields"

commandFields :: Command -> [Text]
commandFields c = case c of
  InsertSegment i s -> "insert-segment" : int i : segmentFields s
  RemoveSegment (SegmentId n) -> ["remove-segment", word n]
  InsertToken (SegmentId s) i t -> "insert-token" : word s : int i : tokenFields t
  RemoveToken (TokenId n) -> ["remove-token", word n]
  SetSurface (TokenId n) s -> ["set-surface", word n, s]
  SplitToken (TokenId n) k j (TokenId m) -> ["split-token", word n, int k, int j, word m]
  MergeToken (TokenId n) -> ["merge-token", word n]
  MoveToken (TokenId n) (SegmentId s) i -> ["move-token", word n, word s, int i]
  InsertMorph (TokenId t) i m -> "insert-morph" : word t : int i : morphFields m
  RemoveMorph (MorphId n) -> ["remove-morph", word n]
  SetForm (MorphId n) f -> ["set-form", word n, f]
  SetGloss (MorphId n) g -> "set-gloss" : word n : glossFields g
  SplitMorph (MorphId n) k (MorphId m) -> ["split-morph", word n, int k, word m]
  MergeMorph (MorphId n) -> ["merge-morph", word n]
  Batch cs -> "batch" : int (length cs) : concatMap commandFields cs

segmentFields :: Segment -> [Text]
segmentFields (Segment (SegmentId n) ts) = word n : int (length ts) : concatMap tokenFields ts

tokenFields :: Token -> [Text]
tokenFields (Token (TokenId n) s ms) = word n : s : int (length ms) : concatMap morphFields ms

morphFields :: Morph -> [Text]
morphFields (Morph (MorphId n) f g) = word n : f : glossFields g

glossFields :: Maybe Gloss -> [Text]
glossFields Nothing = ["0"]
glossFields (Just (Gloss l tags)) = "1" : l : int (length tags) : tags

int :: Int -> Text
int = T.pack . show

word :: Word64 -> Text
word = T.pack . show

commandP :: Parser Command
commandP = do
  tag <- field
  case tag of
    "insert-segment" -> InsertSegment <$> intP <*> segmentP
    "remove-segment" -> RemoveSegment . SegmentId <$> wordP
    "insert-token" -> InsertToken . SegmentId <$> wordP <*> intP <*> tokenP
    "remove-token" -> RemoveToken . TokenId <$> wordP
    "set-surface" -> SetSurface . TokenId <$> wordP <*> field
    "split-token" -> SplitToken . TokenId <$> wordP <*> intP <*> intP <*> (TokenId <$> wordP)
    "merge-token" -> MergeToken . TokenId <$> wordP
    "move-token" -> MoveToken . TokenId <$> wordP <*> (SegmentId <$> wordP) <*> intP
    "insert-morph" -> InsertMorph . TokenId <$> wordP <*> intP <*> morphP
    "remove-morph" -> RemoveMorph . MorphId <$> wordP
    "set-form" -> SetForm . MorphId <$> wordP <*> field
    "set-gloss" -> SetGloss . MorphId <$> wordP <*> glossP
    "split-morph" -> SplitMorph . MorphId <$> wordP <*> intP <*> (MorphId <$> wordP)
    "merge-morph" -> MergeMorph . MorphId <$> wordP
    "batch" -> Batch <$> counted commandP
    _ -> failP ("unknown command: " ++ T.unpack tag)

segmentP :: Parser Segment
segmentP = Segment . SegmentId <$> wordP <*> counted tokenP

tokenP :: Parser Token
tokenP = do
  n <- wordP
  surface <- field
  Token (TokenId n) surface <$> counted morphP

morphP :: Parser Morph
morphP = Morph . MorphId <$> wordP <*> field <*> glossP

glossP :: Parser (Maybe Gloss)
glossP = do
  flag <- field
  case flag of
    "0" -> pure Nothing
    "1" -> do
      lexical <- field
      Just . Gloss lexical <$> counted field
    _ -> failP "bad gloss flag"
