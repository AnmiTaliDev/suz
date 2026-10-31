-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Uzbek
  ( uzbek
  ) where

import Suz.Language
import Suz.Model

-- | A small Uzbek weight table in the Latin script: the pronoun men, the
-- stems kitob, o'qi and uy, and endings for the plural, the accusative,
-- dative, locative, ablative and genitive cases, the first person singular
-- possessive, negation, the past tense and the first person singular, second
-- person singular and first person plural forms. Importing splits the
-- punctuation .,!? and lowercases.
uzbek :: Language
uzbek =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Uzbek"
      ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
      ( [("men", Gloss "" ["1SG"], 10)]
          ++ [("kitob", Gloss "book" [], 10), ("o'qi", Gloss "read" [], 10), ("uy", Gloss "house" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ni"]]
          ++ [(f, Gloss "" ["DAT"], 8) | f <- ["ga", "ka", "qa"]]
          ++ [(f, Gloss "" ["LOC"], 8) | f <- ["da"]]
          ++ [(f, Gloss "" ["ABL"], 8) | f <- ["dan"]]
          ++ [(f, Gloss "" ["GEN"], 8) | f <- ["ning"]]
          ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["im"]]
          ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ma"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["di"]]
          ++ [(f, Gloss "" ["1SG"], 9) | f <- ["m"]]
          ++ [(f, Gloss "" ["2SG"], 9) | f <- ["ng"]]
          ++ [(f, Gloss "" ["1PL"], 9) | f <- ["k"]]
      )
