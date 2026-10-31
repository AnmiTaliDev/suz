-- SPDX-License-Identifier: GPL-3.0-or-later
module Uzbek
  ( uzbek
  ) where

import Suz

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
