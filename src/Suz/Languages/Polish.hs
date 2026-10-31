-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Polish
  ( polish
  ) where

import Suz.Language
import Suz.Model

polish :: Language
polish =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Polish"
      ["1SG", "PRS", "PL", "ACC"]
      ( [("czyta", Gloss "read" [], 10)]
          ++ [("książk", Gloss "book" [], 10)]
          ++ [("m", Gloss "" ["1SG", "PRS"], 9)]
          ++ [("i", Gloss "" ["PL", "ACC"], 9)]
      )
