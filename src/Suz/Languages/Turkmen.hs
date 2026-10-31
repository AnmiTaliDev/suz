-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Turkmen
  ( turkmen
  ) where

import Suz.Language
import Suz.Model

-- | A small Turkmen weight table in the Latin script: the pronoun men, the
-- stems kitap, kitab, oka and öý, and endings for the plural, the accusative,
-- dative, locative, ablative and genitive cases, the first person singular
-- possessive, negation, the past tense and the first person singular, second
-- person singular and first person plural forms. Importing splits the
-- punctuation .,!? and lowercases.
turkmen :: Language
turkmen =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Turkmen"
      ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
      ( [("men", Gloss "" ["1SG"], 10)]
          ++ [("kitap", Gloss "book" [], 10), ("kitab", Gloss "book" [], 10), ("oka", Gloss "read" [], 10), ("öý", Gloss "house" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "ler"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["y", "i", "ny", "ni"]]
          ++ [(f, Gloss "" ["DAT"], 8) | f <- ["a", "e"]]
          ++ [(f, Gloss "" ["LOC"], 8) | f <- ["da", "de"]]
          ++ [(f, Gloss "" ["ABL"], 8) | f <- ["dan", "den"]]
          ++ [(f, Gloss "" ["GEN"], 8) | f <- ["yň", "iň"]]
          ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ym", "im"]]
          ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ma", "me"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dy", "di"]]
          ++ [(f, Gloss "" ["1SG"], 9) | f <- ["m"]]
          ++ [(f, Gloss "" ["2SG"], 9) | f <- ["ň"]]
          ++ [(f, Gloss "" ["1PL"], 9) | f <- ["k"]]
      )
