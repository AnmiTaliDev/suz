-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Tajik
  ( tajik
  ) where

import Suz.Language
import Suz.Model

-- | A small Tajik weight table: the pronoun ман, the stems китоб and хон, the
-- plural ending ҳо, the object marker ро, the imperfective prefix ме and the
-- first person singular ending ам. Importing splits the punctuation .,!? and
-- lowercases.
tajik :: Language
tajik =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Tajik"
      ["1SG", "PL", "ACC", "IPFV"]
      ( [("ман", Gloss "" ["1SG"], 10)]
          ++ [("китоб", Gloss "book" [], 10)]
          ++ [("хон", Gloss "read" [], 10)]
          ++ [("ҳо", Gloss "" ["PL"], 9)]
          ++ [("ро", Gloss "" ["ACC"], 9)]
          ++ [("ме", Gloss "" ["IPFV"], 9)]
          ++ [("ам", Gloss "" ["1SG"], 9)]
      )
