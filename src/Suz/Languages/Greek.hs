-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Greek
  ( greek
  ) where

import Suz.Language
import Suz.Model

greek :: Language
greek =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Greek"
      ["1SG", "PRS", "PL", "NOM", "ACC"]
      ( [("διαβάζ", Gloss "read" [], 10)]
          ++ [("βιβλί", Gloss "book" [], 10)]
          ++ [("ω", Gloss "" ["1SG", "PRS"], 9)]
          ++ [("α", Gloss "" ["NOM", "ACC", "PL"], 9)]
      )
