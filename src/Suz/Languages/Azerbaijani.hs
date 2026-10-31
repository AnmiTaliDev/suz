-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Azerbaijani
  ( azerbaijani
  ) where

import Suz.Language
import Suz.Model

-- | A small Azerbaijani weight table in the Latin script: the pronoun mən,
-- the stems kitab, oxu and işıq, and the plural, accusative, past tense and
-- first person singular endings. Importing splits the punctuation .,!? and
-- lowercases with the Turkic mapping.
azerbaijani :: Language
azerbaijani =
  withImportRules ".,!?" TurkicFolding $
    mkLanguage
      "Azerbaijani"
      ["SG", "PL", "ACC", "PST", "1SG"]
      ( [("mən", Gloss "" ["1SG"], 10)]
          ++ [("kitab", Gloss "book" [], 10), ("oxu", Gloss "read" [], 10), ("işıq", Gloss "light" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "lər"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ı", "i", "u", "ü"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dı", "di", "du", "dü", "tı", "ti", "tu", "tü"]]
          ++ [("m", Gloss "" ["1SG"], 9)]
      )
