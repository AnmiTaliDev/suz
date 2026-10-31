-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Validate (checks) where

import Fixtures
import Suz

badTagDoc :: Document
badTagDoc = after (SetGloss mA1 (Just (Gloss "x" ["ACC", "PL", "FOO"])))

twoBadTagsDoc :: Document
twoBadTagsDoc =
  fst (must (apply (SetGloss mA2 (Just (Gloss "" ["BAR"]))) badTagDoc))

checks :: [(String, Bool)]
checks =
  [ ("validate accepts declared tags", null (validateDocument tinyLanguage baseDoc))
  , ("validate skips unglossed morphs", null (validateDocument tinyLanguage plainDoc))
  , ("validate reports unknown tags", validateDocument tinyLanguage badTagDoc == [Violation mA1 ["ACC", "FOO"]])
  , ("validate keeps document order", map violationMorph (validateDocument tinyLanguage twoBadTagsDoc) == [mA1, mA2])
  , ("validate with empty language", map violationMorph (validateDocument (mkLanguage "e" [] []) baseDoc) == [mA2])
  , ("validateUd of a document without annotations is empty", validateUd baseDoc == [] && validateUd emptyDocument == [])
  , ("validateUd accepts heads inside the segment", validateUd (withHeads [(mA1, HeadOf mA2), (mA2, HeadOf mB), (mB, RootHead)] baseDoc) == [])
  , ("validateUd ignores missing and root heads", validateUd (withHeads [(mA1, NoHead), (mA2, RootHead)] baseDoc) == [])
  , ("validateUd reports a dangling head", validateUd (withHeads [(mA1, HeadOf (MorphId 999))] baseDoc) == [UdDanglingHead mA1 (MorphId 999)])
  , ("validateUd reports a head in another segment", validateUd (withHeads [(mA1, HeadOf mZ)] twoSegmentDoc) == [UdHeadOutsideSegment mA1 mZ])
  , ("validateUd reports a morph that is its own head", validateUd (withHeads [(mA1, HeadOf mA1)] baseDoc) == [UdHeadCycle mA1])
  , ("validateUd reports every morph of a cycle", validateUd (withHeads [(mA1, HeadOf mA2), (mA2, HeadOf mA1)] baseDoc) == [UdHeadCycle mA1, UdHeadCycle mA2])
  , ("validateUd does not report a morph that only leads into a cycle", validateUd (withHeads [(mA1, HeadOf mA2), (mA2, HeadOf mB), (mB, HeadOf mA2)] baseDoc) == [UdHeadCycle mA2, UdHeadCycle mB])
  , ("validateUd lists the problems in document order", validateUd (withHeads [(mB, HeadOf (MorphId 998)), (mA1, HeadOf (MorphId 999))] baseDoc) == [UdDanglingHead mA1 (MorphId 999), UdDanglingHead mB (MorphId 998)])
  ]
