-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Azerbaijani
  ( azerbaijani
  ) where

import Suz.Language
import Suz.Model

-- | A small Azerbaijani weight table in the Latin script: the pronoun mən,
-- the stems kitab, oxu, işıq and ev, and endings for the plural, the
-- accusative, dative, locative, ablative and genitive cases, the first person
-- singular possessive, negation, the past tense and the first person
-- singular, second person singular and first person plural forms. Importing
-- splits the punctuation .,!? and lowercases with the Turkic mapping.
azerbaijani :: Language
azerbaijani =
  withImportRules ".,!?" TurkicFolding $
    mkLanguage
      "Azerbaijani"
      ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
      ( [("mən", Gloss "" ["1SG"], 10)]
          ++ [("kitab", Gloss "book" [], 10), ("oxu", Gloss "read" [], 10), ("işıq", Gloss "light" [], 10), ("ev", Gloss "house" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "lər"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ı", "i", "u", "ü"]]
          ++ [(f, Gloss "" ["DAT"], 8) | f <- ["a", "ə", "ya", "yə"]]
          ++ [(f, Gloss "" ["LOC"], 8) | f <- ["da", "də"]]
          ++ [(f, Gloss "" ["ABL"], 8) | f <- ["dan", "dən"]]
          ++ [(f, Gloss "" ["GEN"], 8) | f <- ["ın", "in", "un", "ün", "nın", "nin", "nun", "nün"]]
          ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ım", "im", "um", "üm"]]
          ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ma", "mə"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dı", "di", "du", "dü"]]
          ++ [(f, Gloss "" ["1SG"], 9) | f <- ["m"]]
          ++ [(f, Gloss "" ["2SG"], 9) | f <- ["n"]]
          ++ [(f, Gloss "" ["1PL"], 9) | f <- ["q", "k"]]
      )
