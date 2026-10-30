-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Segment (checks) where

import Fixtures
import Suz

segLanguage :: Language
segLanguage =
  mkLanguage
    "seg"
    ["PL", "ACC"]
    [ ("китап", glossBook, 3)
    , ("лар", glossPl, 3)
    , ("ны", Gloss "" ["ACC"], 1)
    , ("ab", glossPl, 1)
    , ("abab", glossPl, 1)
    , ("a", glossPl, 1)
    , ("b", glossPl, 1)
    , ("c", glossPl, 1)
    , ("bc", glossPl, 6)
    , ("neg", glossPl, -1)
    ]

appliedSegmentation :: Language -> Document -> Document
appliedSegmentation lang doc = fst (must (apply (Batch (suggestSegmentation lang doc)) doc))

checks :: [(String, Bool)]
checks =
  [ ("segment known pair", segmentForm segLanguage "китаплар" == Just ["китап", "лар"])
  , ("segment three pieces", segmentForm segLanguage "китапларны" == Just ["китап", "лар", "ны"])
  , ("segment whole known form", segmentForm segLanguage "китап" == Just ["китап"])
  , ("segment unknown remainder", segmentForm segLanguage "китапзз" == Nothing)
  , ("segment unknown form", segmentForm segLanguage "мин" == Nothing)
  , ("segment empty form", segmentForm segLanguage "" == Nothing)
  , ("segment prefers fewer pieces", segmentForm segLanguage "abab" == Just ["abab"] && segmentForm segLanguage "ab" == Just ["ab"])
  , ("segment prefers heavier on equal count", segmentForm segLanguage "abc" == Just ["a", "bc"])
  , ("segment ignores non-positive forms", segmentForm segLanguage "neg" == Nothing)
  , ("suggest segmentation splits single morphs", length (suggestSegmentation segLanguage segmentationDoc) == 1)
  , ("suggested segmentation forms", forms (appliedSegmentation segLanguage segmentationDoc) == ["китап", "лар", "мин", "лар"])
  , ("suggested segmentation is undone", roundTripsIn segmentationDoc (Batch (suggestSegmentation segLanguage segmentationDoc)))
  , ("suggested segmentation ids are fresh", all (\c -> case c of SplitMorph _ _ (MorphId n) -> n >= nextFreeId segmentationDoc; _ -> False) (suggestSegmentation segLanguage segmentationDoc))
  , ("suggest segmentation with three pieces", length (suggestSegmentation segLanguage (appliedWith "китапларны")) == 2)
  , ("suggest segmentation skips segmented tokens", null (suggestSegmentation segLanguage baseDoc))
  , ("suggest segmentation skips glossed morphs", null (suggestSegmentation segLanguage (after (SetGloss mA1 (Just glossBook)))))
  ]
  where
    appliedWith surface =
      fst (must (apply (InsertSegment 0 (buildSegment emptyDocument [(surface, [surface])])) emptyDocument))
