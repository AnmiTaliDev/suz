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
  ]
