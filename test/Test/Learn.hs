-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Learn (checks) where

import Fixtures
import Suz

learnSegment :: Segment
learnSegment = buildSegment emptyDocument [("cats", ["cat", "s"])]

learnDoc :: Document
learnDoc = fst (must (apply (InsertSegment 0 learnSegment) emptyDocument))

mCat, mS :: MorphId
mCat = morphId (tokenMorphs (segmentTokens learnSegment !! 0) !! 0)
mS = morphId (tokenMorphs (segmentTokens learnSegment !! 0) !! 1)

learnBase :: Language
learnBase = mkLanguage "l" ["PL", "3SG"] []

learnSession :: Session
learnSession = newSession learnDoc

learned :: Language -> [Command] -> Language
learned lang = snd . foldl step (learnSession, lang)
  where
    step (session, l) c = must (performLearning c l session)

checks :: [(String, Bool)]
checks =
  [ ( "learn confirms a new gloss with context"
    , let l = learned learnBase [SetGloss mS (Just glossPl)]
       in candidates l "s" == [(glossPl, 1)] && contextCandidates l "cat" "s" == [(glossPl, 1)]
    )
  , ( "learn has no context for the first morph"
    , let l = learned learnBase [SetGloss mCat (Just glossBook)]
       in candidates l "cat" == [(glossBook, 1)] && null (languageContext l)
    )
  , ( "learn replaces a gloss"
    , let l = learned learnBase [SetGloss mS (Just glossPl), SetGloss mS (Just glossSg3)]
       in candidates l "s" == [(glossSg3, 1), (glossPl, 0)] && contextCandidates l "cat" "s" == [(glossSg3, 1), (glossPl, 0)]
    )
  , ( "learn ignores an unchanged gloss"
    , learned learnBase [SetGloss mS (Just glossPl), SetGloss mS (Just glossPl)] == learned learnBase [SetGloss mS (Just glossPl)]
    )
  , ( "learn rejects a cleared gloss"
    , let l = learned learnBase [SetGloss mS (Just glossPl), SetGloss mS Nothing]
       in candidates l "s" == [(glossPl, 0)] && contextCandidates l "cat" "s" == [(glossPl, 0)]
    )
  , ( "learn from a batch"
    , let l = learned learnBase [Batch [SetGloss mCat (Just glossBook), SetGloss mS (Just glossPl)]]
       in candidates l "cat" == [(glossBook, 1)] && candidates l "s" == [(glossPl, 1)]
    )
  , ( "learn rejects with the old form"
    , let l = learned learnBase [SetGloss mS (Just glossPl), Batch [SetForm mS "es", SetGloss mS (Just glossSg3)]]
       in candidates l "s" == [(glossPl, 0)] && candidates l "es" == [(glossSg3, 1)] && contextCandidates l "cat" "es" == [(glossSg3, 1)]
    )
  , ( "learn from an inserted glossed morph"
    , let l = learned learnBase [InsertMorph (tokenId (segmentTokens learnSegment !! 0)) 2 (Morph (MorphId (nextFreeId learnDoc)) "x" (Just glossPl))]
       in candidates l "x" == [(glossPl, 1)] && contextCandidates l "s" "x" == [(glossPl, 1)]
    )
  , ( "learn ignores commands without glosses"
    , learned learnBase [SetForm mS "es", SetSurface (tokenId (segmentTokens learnSegment !! 0)) "q"] == learnBase
    )
  , ("learn from identical documents", learnChange baseDoc baseDoc tinyLanguage == tinyLanguage)
  , ( "failed command teaches nothing"
    , fmap (const ()) (performLearning (SetGloss (MorphId 999) (Just glossPl)) learnBase learnSession) == Left (NoSuchMorph (MorphId 999))
    )
  , ( "learning loop fixes the suggestion"
    , let lang = mkLanguage "l" ["PL", "3SG"] [("cat", glossBook, 1), ("s", glossPl, 1), ("s", glossSg3, 0.5)]
          suggested = must (perform (Batch (suggestGlosses lang learnDoc)) learnSession)
          (fixed, lang') = must (performLearning (SetGloss mS (Just glossSg3)) lang suggested)
       in suggestAfter lang (Just "cat") "s" == Just glossPl
            && suggestAfter lang' (Just "cat") "s" == Just glossSg3
            && fmap morphGloss (lookupMorph mS (sessionDocument fixed)) == Just (Just glossSg3)
    )
  ]
  where
    lookupMorph mid doc = fmap (\(_, _, m) -> m) (findMorph mid doc)
