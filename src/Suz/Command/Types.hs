-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Command.Types
  ( Command (..)
  , EditError (..)
  ) where

import Data.Text (Text)
import Data.Word (Word64)
import Suz.Model

-- | An editing operation, applied with 'Suz.Command.apply'. Positions count from
-- 0 and a position equal to the length of the list inserts at the end.
data Command
  = InsertSegment Int Segment
  | RemoveSegment SegmentId
  | InsertToken SegmentId Int Token
  | RemoveToken TokenId
  | SetSurface TokenId Text
  | SplitToken TokenId Int Int TokenId
  | MergeToken TokenId
  | MoveToken TokenId SegmentId Int
  | InsertMorph TokenId Int Morph
  | RemoveMorph MorphId
  | SetForm MorphId Text
  | SetGloss MorphId (Maybe Gloss)
  | SplitMorph MorphId Int MorphId
  | MergeMorph MorphId
  | Batch [Command]
  deriving (Eq, Show)

data EditError
  = NoSuchSegment SegmentId
  | NoSuchToken TokenId
  | NoSuchMorph MorphId
  | IndexOutOfRange Int
  | DuplicateId Word64
  | BadSplitPoint MorphId Int
  | NoNextMorph MorphId
  | BadTokenSplit TokenId Int Int
  | NoNextToken TokenId
  deriving (Eq, Show)
