-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Turkish
  ( turkish
  ) where

import Suz.Language
import Suz.Model

-- | A small Turkish weight table: the pronoun ben, the stems kitap, oku and
-- ışık, and the plural, accusative, past tense and first person singular
-- endings. Importing splits the punctuation .,!? and lowercases with the
-- Turkic mapping.
turkish :: Language
turkish =
  withImportRules ".,!?" TurkicFolding $
    mkLanguage
      "Turkish"
      ["SG", "PL", "ACC", "PST", "1SG"]
      ( [("ben", Gloss "" ["1SG"], 10)]
          ++ [("kitap", Gloss "book" [], 10), ("oku", Gloss "read" [], 10), ("ışık", Gloss "light" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "ler"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ı", "i", "u", "ü"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dı", "di", "du", "dü", "tı", "ti", "tu", "tü"]]
          ++ [("m", Gloss "" ["1SG"], 9)]
      )
