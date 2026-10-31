-- SPDX-License-Identifier: GPL-3.0-or-later
module Turkmen
  ( turkmen
  ) where

import Suz

turkmen :: Language
turkmen =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Turkmen"
      ["SG", "PL", "ACC", "PST", "1SG"]
      ( [("men", Gloss "" ["1SG"], 10)]
          ++ [("kitap", Gloss "book" [], 10), ("oka", Gloss "read" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "ler"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["y", "i", "ny", "ni"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dy", "di"]]
          ++ [("m", Gloss "" ["1SG"], 9)]
      )
