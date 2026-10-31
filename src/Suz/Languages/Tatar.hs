-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Tatar
  ( tatar
  ) where

import Suz.Language
import Suz.Model

tatar :: Language
tatar =
  mkLanguage
    "Tatar"
    ["SG", "1SG", "PL", "3PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
    ( [("мин", Gloss "" ["1SG"], 10)]
        ++ [("китап", Gloss "book" [], 10), ("укы", Gloss "read" [], 10), ("өй", Gloss "house" [], 10)]
        ++ [(f, Gloss "" ["PL"], 8) | f <- ["лар", "ләр", "нар", "нәр"]]
        ++ [(f, Gloss "" ["3PL"], 2) | f <- ["лар", "ләр"]]
        ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ны", "не", "н"]]
        ++ [(f, Gloss "" ["DAT"], 8) | f <- ["га", "гә", "ка", "кә", "на", "нә"]]
        ++ [(f, Gloss "" ["LOC"], 8) | f <- ["да", "дә", "та", "тә"]]
        ++ [(f, Gloss "" ["ABL"], 8) | f <- ["дан", "дән", "тан", "тән", "нан", "нән"]]
        ++ [(f, Gloss "" ["GEN"], 8) | f <- ["ның", "нең"]]
        ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ым", "ем"]]
        ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ма", "мә"]]
        ++ [(f, Gloss "" ["PST"], 9) | f <- ["ды", "де", "ты", "те"]]
        ++ [(f, Gloss "" ["1SG"], 9) | f <- ["м"]]
        ++ [(f, Gloss "" ["2SG"], 9) | f <- ["ң"]]
        ++ [(f, Gloss "" ["1PL"], 9) | f <- ["к"]]
    )
