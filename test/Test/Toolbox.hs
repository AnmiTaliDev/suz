-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Toolbox (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("toolbox text", renderToolbox baseDoc == "\\ref 0\n\\tx китаплар мин\n\\mb китап-лар мин\n\\ge book-PL ?\n")
  , ("toolbox round trip", parseToolbox tinyLanguage (renderToolbox baseDoc) == Right baseDoc)
  , ("toolbox round trip of an empty segment", parseToolbox tinyLanguage (renderToolbox emptySegmentDoc) == Right emptySegmentDoc)
  , ("toolbox gloss with lexical part and tags", toolboxGlosses "\\tx a\n\\mb a\n\\ge walk.3SG\n" == Right [Just (Gloss "walk" ["3SG"])])
  , ("toolbox gloss with a tag only", toolboxGlosses "\\tx a\n\\mb a\n\\ge 3SG\n" == Right [Just (Gloss "" ["3SG"])])
  , ("toolbox without ge is unglossed", toolboxGlosses "\\tx a\n\\mb a-b\n" == Right [Nothing, Nothing])
  , ("toolbox without mb has one morph per token", fmap (map morphForm . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseToolbox tinyLanguage "\\tx a b\n") == Right ["a", "b"])
  , ("toolbox skips headers and unknown markers", fmap (length . documentSegments) (parseToolbox tinyLanguage "\\_sh v3.0\n\n\\ref 1\n\\tx a\n\\nt note\n\n\n\\tx b\n") == Right 2)
  , ("toolbox mb must match tx", errorOf (parseToolbox tinyLanguage "\\tx a b\n\\mb a\n") == Just (2, "\\mb does not match \\tx"))
  , ("toolbox ge must match mb", errorOf (parseToolbox tinyLanguage "\n\\tx a\n\\mb a-b\n\\ge x\n") == Just (4, "\\ge does not match \\mb"))
  , ("toolbox rejects a line without a marker", errorOf (parseToolbox tinyLanguage "\\tx a\nb\n") == Just (2, "line out of place"))
  , ("toolbox import is valid", fmap (null . validateDocument tinyLanguage) (parseToolbox tinyLanguage (renderToolbox baseDoc)) == Right True)
  ]
  where
    emptySegmentDoc = fst (must (apply (InsertSegment 0 (buildSegment emptyDocument [])) emptyDocument))
    toolboxGlosses t = fmap (map morphGloss . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseToolbox tinyLanguage t)
