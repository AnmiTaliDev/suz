-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Uzbek
  ( uzbek
  ) where

import Suz.Language
import Suz.Model

-- | A small Uzbek weight table in the Latin script: the pronoun men, the
-- stems kitob and o'qi, and the plural, accusative, past tense and first
-- person singular endings. Importing splits the punctuation .,!? and
-- lowercases.
uzbek :: Language
uzbek =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Uzbek"
      ["SG", "PL", "ACC", "PST", "1SG"]
      ( [("men", Gloss "" ["1SG"], 10)]
          ++ [("kitob", Gloss "book" [], 10), ("o'qi", Gloss "read" [], 10)]
          ++ [("lar", Gloss "" ["PL"], 8)]
          ++ [("ni", Gloss "" ["ACC"], 9)]
          ++ [("di", Gloss "" ["PST"], 9)]
          ++ [("m", Gloss "" ["1SG"], 9)]
      )
