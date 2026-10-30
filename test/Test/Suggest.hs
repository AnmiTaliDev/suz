-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Suggest (checks) where

import Fixtures
import Suz

contextDoc :: Document
contextDoc =
  fst
    ( must
        ( apply
            (InsertSegment 0 (buildSegment emptyDocument [("cats", ["cat", "s"]), ("walks", ["walk", "s"]), ("s", ["s"])]))
            emptyDocument
        )
    )

contextGlossLanguage :: Language
contextGlossLanguage =
  withContext
    [("walk", "s", glossSg3, 2)]
    (mkLanguage "ctx" ["PL", "3SG"] [("cat", glossBook, 1), ("walk", Gloss "walk" [], 1), ("s", glossPl, 5.5), ("s", glossSg3, 5)])

checks :: [(String, Bool)]
checks =
  [ ("suggest fills unglossed morphs", suggestions == 2)
  , ("suggest skips glossed morphs", null (suggestGlosses tinyLanguage baseDoc))
  , ("suggested glosses apply", glossesAfterSuggest == [Just glossBook, Just glossPl, Nothing])
  , ( "suggested glosses use context"
    , [g | Just g <- map (\c -> case c of SetGloss _ g' -> g'; _ -> Nothing) (suggestGlosses contextGlossLanguage contextDoc)]
        == [glossBook, glossPl, Gloss "walk" [], glossSg3, glossPl]
    )
  ]
  where
    suggestions = length (suggestGlosses tinyLanguage plainDoc)
    glossesAfterSuggest =
      let doc = fst (must (apply (Batch (suggestGlosses tinyLanguage plainDoc)) plainDoc))
       in [morphGloss m | s <- documentSegments doc, t <- segmentTokens s, m <- tokenMorphs t]
