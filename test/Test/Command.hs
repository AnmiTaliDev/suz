-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Command (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("every command is undone by its inverse", all roundTrips commands)
  , ("split forms", forms (after (SplitMorph mA1 2 spareId)) == ["ки", "тап", "лар", "мин"])
  , ("merge forms", forms (after (MergeMorph mA1)) == ["китаплар", "мин"])
  , ("merge keeps first gloss", firstGloss (after (MergeMorph mA1)) == Just glossBook)
  , ("remove then insert restores position", roundTrips (RemoveToken (tokenId tokA)))
  , ("unknown token", errorOf (apply (RemoveToken (TokenId 999)) baseDoc) == Just (NoSuchToken (TokenId 999)))
  , ( "duplicate id"
    , errorOf (apply (InsertToken (segmentId plainSegment) 0 tokA) baseDoc)
        == Just (DuplicateId (rawToken tokA))
    )
  , ( "index out of range"
    , errorOf (apply (InsertToken (segmentId plainSegment) 5 (buildToken baseDoc "q" [])) baseDoc)
        == Just (IndexOutOfRange 5)
    )
  , ("split at zero", errorOf (apply (SplitMorph mA1 0 spareId) baseDoc) == Just (BadSplitPoint mA1 0))
  , ("split at end", errorOf (apply (SplitMorph mA1 5 spareId) baseDoc) == Just (BadSplitPoint mA1 5))
  , ("merge last morph", errorOf (apply (MergeMorph mA2) baseDoc) == Just (NoNextMorph mA2))
  , ( "failed batch changes nothing"
    , errorOf (apply (Batch [SetForm mA1 "z", RemoveMorph (MorphId 999)]) baseDoc) == Just (NoSuchMorph (MorphId 999))
    )
  , ("ids are not reused after removal", nextFreeId (after (RemoveSegment (segmentId plainSegment))) == nextFreeId baseDoc)
  , ("split token surfaces", tokenSurfaces (after (SplitToken (tokenId tokA) 1 5 splitTokenId)) == [["китап", "лар", "мин"]])
  , ("split token keeps morph order", forms (after (SplitToken (tokenId tokA) 1 5 splitTokenId)) == ["китап", "лар", "мин"])
  , ("split token moves morphs", map (length . tokenMorphs) (segmentTokens (documentSegments (after (SplitToken (tokenId tokA) 1 5 splitTokenId)) !! 0)) == [1, 1, 1])
  , ("split token at the ends", all (roundTrips . (\(k, j) -> SplitToken (tokenId tokA) k j splitTokenId)) [(0, 0), (2, 8), (0, 8), (2, 0)])
  , ("merge token surfaces", tokenSurfaces (after (MergeToken (tokenId tokA))) == [["китаплармин"]])
  , ("merge token keeps first id", fmap (\d -> map tokenId (segmentTokens (documentSegments d !! 0))) (fmap fst (apply (MergeToken (tokenId tokA)) baseDoc)) == Right [tokenId tokA])
  , ("move token within segment", tokenSurfaces (after (MoveToken (tokenId tokB) (segmentId plainSegment) 0)) == [["мин", "китаплар"]])
  , ("move token to another segment", tokenSurfaces (fst (must (apply (MoveToken (tokenId tokA) (segmentId otherSegment) 1) twoSegmentDoc))) == [["мин"], ["z", "китаплар"]])
  , ("move token across segments is undone", roundTripsIn twoSegmentDoc (MoveToken (tokenId tokA) (segmentId otherSegment) 0))
  , ("split token bad morph index", errorOf (apply (SplitToken (tokenId tokA) 3 0 splitTokenId) baseDoc) == Just (BadTokenSplit (tokenId tokA) 3 0))
  , ("split token bad surface index", errorOf (apply (SplitToken (tokenId tokA) 0 9 splitTokenId) baseDoc) == Just (BadTokenSplit (tokenId tokA) 0 9))
  , ("split token negative index", errorOf (apply (SplitToken (tokenId tokA) (-1) 0 splitTokenId) baseDoc) == Just (BadTokenSplit (tokenId tokA) (-1) 0))
  , ("split token duplicate id", errorOf (apply (SplitToken (tokenId tokA) 1 1 (tokenId tokB)) baseDoc) == Just (DuplicateId (rawToken tokB)))
  , ("merge last token", errorOf (apply (MergeToken (tokenId tokB)) baseDoc) == Just (NoNextToken (tokenId tokB)))
  , ("move token to unknown segment", errorOf (apply (MoveToken (tokenId tokA) (SegmentId 999) 0) baseDoc) == Just (NoSuchSegment (SegmentId 999)))
  , ("move token out of range", errorOf (apply (MoveToken (tokenId tokA) (segmentId plainSegment) 5) baseDoc) == Just (IndexOutOfRange 5))
  , ("set ud stores the annotation", udOf mA1 udBase == Just (Just udSample))
  , ("set ud with nothing clears the annotation", udOf mA1 (fst (must (apply (SetUd mA1 Nothing) udBase))) == Just Nothing)
  , ("set ud is undone by its inverse", roundTripsIn udBase (SetUd mA1 Nothing) && roundTrips (SetUd mA1 (Just udSample)))
  , ("set ud unknown morph", errorOf (apply (SetUd (MorphId 999) Nothing) baseDoc) == Just (NoSuchMorph (MorphId 999)))
  , ("split keeps the annotation on the first part", udOf mA1 (fst (must (apply (SplitMorph mA1 2 spareId) udBase))) == Just (Just udSample))
  , ("split leaves the new part without annotation", udOf spareId (fst (must (apply (SplitMorph mA1 2 spareId) udBase))) == Just Nothing)
  , ("merge keeps the annotation of the first morph", udOf mA1 (fst (must (apply (MergeMorph mA1) udBase))) == Just (Just udSample {udHead = NoHead}))
  , ("removing an annotated morph is undone", roundTripsIn udBase (RemoveMorph mA1))
  , ("removing a morph clears the heads that point at it", udHeadOf mB (after' (RemoveMorph mA2) headDoc) == Just NoHead)
  , ("removing a morph keeps the heads that point elsewhere", udHeadOf mA1 (after' (RemoveMorph mB) headDoc) == Just (HeadOf mA2))
  , ("removing a morph with heads is undone", roundTripsIn headDoc (RemoveMorph mA2))
  , ("removing a morph that nothing depends on has a plain inverse", plainInsert (snd (must (apply (RemoveMorph mA1) baseDoc))))
  , ("removing a token clears the heads that point into it", udHeadOf mB (after' (RemoveToken (tokenId tokA)) headDoc) == Just NoHead)
  , ("removing a token with heads is undone", roundTripsIn headDoc (RemoveToken (tokenId tokA)))
  , ("removing a segment clears the heads that point into it", udHeadOf mZ (after' (RemoveSegment (segmentId plainSegment)) zHeadDoc) == Just NoHead)
  , ("removing a segment with heads is undone", roundTripsIn zHeadDoc (RemoveSegment (segmentId plainSegment)))
  , ("merging morphs points the heads at the merged morph", udHeadOf mB (after' (MergeMorph mA1) headDoc) == Just (HeadOf mA1))
  , ("merging morphs with heads is undone", roundTripsIn headDoc (MergeMorph mA1))
  ]
  where
    headDoc = withHeads [(mA1, HeadOf mA2), (mB, HeadOf mA2)] baseDoc
    zHeadDoc = withHeads [(mZ, HeadOf mA1)] twoSegmentDoc
    after' c d = fst (must (apply c d))
    plainInsert c = case c of
      InsertMorph {} -> True
      _ -> False
