-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Leipzig (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("leipzig round trip", parseLeipzig tinyLanguage (renderDocument baseDoc) == Right baseDoc)
  , ("leipzig two segments", fmap (length . documentSegments) (parseLeipzig tinyLanguage "a b\na b\nx y\n\nc\nc\nz\n") == Right 2)
  , ("leipzig ignores the translation line", parseLeipzig tinyLanguage "a\na\nx\nit\n" == parseLeipzig tinyLanguage "a\na\nx\n")
  , ("leipzig gloss with lexical part and tags", glosses "a\na\nwalk.3SG\n" == Right [Just (Gloss "walk" ["3SG"])])
  , ("leipzig gloss with a tag only", glosses "a\na\n3SG\n" == Right [Just (Gloss "" ["3SG"])])
  , ("leipzig question mark is unglossed", glosses "a\na-b\n?-x\n" == Right [Nothing, Just (Gloss "x" [])])
  , ("leipzig underscore is a token without morphs", fmap (map (length . tokenMorphs) . concatMap segmentTokens . documentSegments) (parseLeipzig tinyLanguage "a b\na _\nx _\n") == Right [1, 0])
  , ("leipzig morph line must match words", errorOf (parseLeipzig tinyLanguage "a b\na\nx\n") == Just (2, "morph line does not match word line"))
  , ("leipzig gloss line must match morphs", errorOf (parseLeipzig tinyLanguage "\na\na-b\nx\n") == Just (4, "gloss line does not match morph line"))
  , ("leipzig block needs three lines", errorOf (parseLeipzig tinyLanguage "a\na\n") == Just (1, "expected 3 or 4 lines"))
  , ("leipzig import is valid", fmap (null . validateDocument tinyLanguage) (parseLeipzig tinyLanguage (renderDocument baseDoc)) == Right True)
  ]
  where
    glosses t = fmap (map morphGloss . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseLeipzig tinyLanguage t)
