-- SPDX-License-Identifier: GPL-3.0-or-later
module Kyrgyz
  ( kyrgyz
  ) where

import Suz

kyrgyz :: Language
kyrgyz =
  withImportRules ".,!?" SimpleFolding $
    withContext [(pl, f, Gloss "" ["ACC"], 6) | pl <- plurals, f <- shared] $
      mkLanguage
        "Kyrgyz"
        ["SG", "PL", "ACC", "PST", "1SG"]
        ( [("мен", Gloss "" ["1SG"], 10)]
            ++ [("китеп", Gloss "book" [], 10), ("оку", Gloss "read" [], 10)]
            ++ [(f, Gloss "" ["PL"], 8) | f <- plurals]
            ++ [(f, Gloss "" ["ACC"], 5) | f <- shared]
            ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ны", "ни", "ну", "нү"]]
            ++ [(f, Gloss "" ["PST"], 9) | f <- shared]
            ++ [("м", Gloss "" ["1SG"], 9)]
        )
  where
    plurals = ["лар", "лер", "дар", "дер", "тар", "тер"]
    shared = ["ды", "ди", "ду", "дү", "ты", "ти", "ту", "тү"]
