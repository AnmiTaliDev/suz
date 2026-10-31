-- SPDX-License-Identifier: GPL-3.0-or-later
module CrimeanTatar
  ( crimeanTatar
  ) where

import Suz

crimeanTatar :: Language
crimeanTatar =
  withImportRules ".,!?" TurkicFolding $
    mkLanguage
      "Crimean Tatar"
      ["SG", "PL", "ACC", "PST", "1SG"]
      ( [("men", Gloss "" ["1SG"], 10)]
          ++ [("kitap", Gloss "book" [], 10), ("oqu", Gloss "read" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "ler"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["nı", "ni"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dı", "di", "du", "dü", "tı", "ti", "tu", "tü"]]
          ++ [("m", Gloss "" ["1SG"], 9)]
      )
