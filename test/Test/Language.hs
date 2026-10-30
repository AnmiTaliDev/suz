-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Language (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("candidates order", candidates tinyLanguage "s" == [(glossPl, 5.5), (Gloss "" ["3SG"], 4)])
  , ("unknown form", null (candidates tinyLanguage "zzz") && suggest tinyLanguage "zzz" == Nothing)
  , ("confirm changes ranking", suggest (confirmTwice tinyLanguage) "s" == Just (Gloss "" ["3SG"]))
  , ("reject lowers the weight", candidates (reject "s" glossPl tinyLanguage) "s" == [(glossPl, 4.5), (Gloss "" ["3SG"], 4)])
  , ("reject changes ranking", suggest (reject "s" glossPl (reject "s" glossPl tinyLanguage)) "s" == Just (Gloss "" ["3SG"]))
  , ("reject creates a negative entry", candidates (reject "q" glossPl tinyLanguage) "q" == [(glossPl, -1)])
  , ("zero weight is not suggested", suggest (reject "q" glossPl (confirm "q" glossPl (mkLanguage "z" [] []))) "q" == Nothing)
  , ("zero weight stays a candidate", candidates (reject "q" glossPl (confirm "q" glossPl (mkLanguage "z" [] []))) "q" == [(glossPl, 0)])
  , ("confirm undoes reject", candidates (confirm "s" glossPl (reject "s" glossPl tinyLanguage)) "s" == candidates tinyLanguage "s")
  , ("negative weight is not suggested", suggest (reject "q" glossPl (reject "q" glossPl (confirm "q" glossPl (mkLanguage "z" [] [])))) "q" == Nothing)
  , ("valid gloss", isValidGloss tinyLanguage glossPl && not (isValidGloss tinyLanguage (Gloss "" ["ACC"])))
  , ("context raw candidates", contextCandidates contextLanguage "walk" "s" == [(glossSg3, 2.5)])
  , ("context adds to plain weights", candidatesAfter contextLanguage (Just "walk") "s" == [(glossSg3, 6.5), (glossPl, 5.5)])
  , ("context after cat", candidatesAfter contextLanguage (Just "cat") "s" == [(glossPl, 8.5), (glossSg3, 4)])
  , ("no previous form equals plain candidates", candidatesAfter contextLanguage Nothing "s" == candidates contextLanguage "s")
  , ("unknown previous form equals plain candidates", candidatesAfter contextLanguage (Just "dog") "s" == candidates contextLanguage "s")
  , ("context-only candidates", candidatesAfter contextLanguage (Just "cat") "zz" == [(glossPl, 1)])
  , ("suggest after", suggestAfter contextLanguage (Just "walk") "s" == Just glossSg3 && suggest contextLanguage "s" == Just glossPl)
  , ("confirm after flips ranking", suggestAfter (confirmAfter "walk" "s" glossSg3 (confirmAfter "walk" "s" glossSg3 tinyLanguage)) (Just "walk") "s" == Just glossSg3)
  , ("confirm after leaves plain weights", candidates (confirmAfter "walk" "s" glossSg3 tinyLanguage) "s" == candidates tinyLanguage "s")
  , ("reject after lowers context weight", contextCandidates (rejectAfter "walk" "s" glossSg3 tinyLanguage) "walk" "s" == [(glossSg3, -1)])
  , ("reject after cancels confirm after", contextCandidates (rejectAfter "p" "q" glossPl (confirmAfter "p" "q" glossPl tinyLanguage)) "p" "q" == [(glossPl, 0)])
  , ("with context sums repeated entries", contextCandidates (withContext [("p", "q", glossPl, 1)] (withContext [("p", "q", glossPl, 2)] tinyLanguage)) "p" "q" == [(glossPl, 3)])
  ]
  where
    confirmTwice = confirm "s" (Gloss "" ["3SG"]) . confirm "s" (Gloss "" ["3SG"])
