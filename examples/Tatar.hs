-- SPDX-License-Identifier: GPL-3.0-or-later
module Tatar
  ( tatar
  ) where

import Suz

tatar :: Language
tatar =
  mkLanguage
    "Tatar"
    ["SG", "PL", "ACC", "PST", "1SG", "3PL"]
    ( [("мин", Gloss "" ["1SG"], 10)]
        ++ [("китап", Gloss "book" [], 10), ("укы", Gloss "read" [], 10)]
        ++ [(f, Gloss "" ["PL"], 8) | f <- ["лар", "ләр", "нар", "нәр"]]
        ++ [(f, Gloss "" ["3PL"], 2) | f <- ["лар", "ләр"]]
        ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ны", "не", "н"]]
        ++ [(f, Gloss "" ["PST"], 9) | f <- ["ды", "де", "ты", "те"]]
        ++ [("м", Gloss "" ["1SG"], 9)]
    )
