-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Validate
  ( Violation (..)
  , validateDocument
  , UdViolation (..)
  , validateUd
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import Suz.Language
import Suz.Model

-- | A glossed morph and the tags of its gloss that the language does not declare.
data Violation = Violation
  { violationMorph :: MorphId
  , violationTags :: [Text]
  }
  deriving (Eq, Show)

-- | Glossed morphs whose gloss has tags the language does not declare, in
-- document order. Each violation lists the unknown tags in gloss order.
-- Morphs without a gloss are skipped.
validateDocument :: Language -> Document -> [Violation]
validateDocument lang doc =
  [ Violation (morphId m) unknown
  | s <- documentSegments doc
  , t <- segmentTokens s
  , m <- tokenMorphs t
  , Just g <- [morphGloss m]
  , let unknown = filter (`Set.notMember` languageTags lang) (glossTags g)
  , not (null unknown)
  ]

-- | A problem with the head of a morph that has a UD annotation: the head is
-- a morph that does not exist, a morph in another segment, or the morph is on
-- a cycle of heads (a morph that is its own head is on one).
data UdViolation
  = UdDanglingHead MorphId MorphId
  | UdHeadOutsideSegment MorphId MorphId
  | UdHeadCycle MorphId
  deriving (Eq, Show)

-- | Problems with the heads of the morphs of a document, in document order.
-- A morph can have several. Removing or merging morphs never leaves a dangling
-- head behind, so these come from data read from outside or from the commands
-- that set or insert an annotation.
validateUd :: Document -> [UdViolation]
validateUd doc = concatMap check heads
  where
    heads :: [(MorphId, SegmentId, MorphId)]
    heads =
      [ (morphId m, segmentId s, h)
      | s <- documentSegments doc
      , t <- segmentTokens s
      , m <- tokenMorphs t
      , Just u <- [morphUd m]
      , HeadOf h <- [udHead u]
      ]
    segmentOf :: Map MorphId SegmentId
    segmentOf = Map.fromList [(morphId m, segmentId s) | s <- documentSegments doc, t <- segmentTokens s, m <- tokenMorphs t]
    headOf :: Map MorphId MorphId
    headOf = Map.fromList [(m, h) | (m, _, h) <- heads]
    check (m, sid, h) =
      [UdDanglingHead m h | Map.notMember h segmentOf]
        ++ [UdHeadOutsideSegment m h | Just other <- [Map.lookup h segmentOf], other /= sid]
        ++ [UdHeadCycle m | onCycle m]
    onCycle m = go Set.empty m
      where
        go seen cur = case Map.lookup cur headOf of
          Nothing -> False
          Just next
            | next == m -> True
            | Set.member next seen -> False
            | otherwise -> go (Set.insert next seen) next
