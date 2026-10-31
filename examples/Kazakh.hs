-- SPDX-License-Identifier: GPL-3.0-or-later
module Kazakh
  ( kazakh
  ) where

import Suz

kazakh :: Language
kazakh =
  withImportRules ".,!?" SimpleFolding $
    withContext [(pl, f, Gloss "" ["ACC"], 6) | pl <- plurals, f <- shared] $
      mkLanguage
        "Kazakh"
        ["SG", "PL", "ACC", "PST", "1SG"]
        ( [("мен", Gloss "" ["1SG"], 10)]
            ++ [("кітап", Gloss "book" [], 10), ("оқы", Gloss "read" [], 10)]
            ++ [(f, Gloss "" ["PL"], 8) | f <- plurals]
            ++ [(f, Gloss "" ["ACC"], 5) | f <- shared]
            ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ны", "ні"]]
            ++ [(f, Gloss "" ["PST"], 9) | f <- shared]
            ++ [("м", Gloss "" ["1SG"], 9)]
        )
  where
    plurals = ["лар", "лер", "дар", "дер", "тар", "тер"]
    shared = ["ды", "ді", "ты", "ті"]
