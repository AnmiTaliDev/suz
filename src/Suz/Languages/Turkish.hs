-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Turkish
  ( turkish
  ) where

import Suz.Language
import Suz.Model

turkish :: Language
turkish =
  withImportRules ".,!?" TurkicFolding $
    mkLanguage
      "Turkish"
      ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "FUT", "PROG", "PST", "2SG", "1PL"]
      ( [("ben", Gloss "" ["1SG"], 10)]
          ++ [("kitap", Gloss "book" [], 10), ("kitab", Gloss "book" [], 10), ("oku", Gloss "read" [], 10), ("ışık", Gloss "light" [], 10), ("ev", Gloss "house" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "ler"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ı", "i", "u", "ü"]]
          ++ [(f, Gloss "" ["DAT"], 8) | f <- ["a", "e", "ya", "ye"]]
          ++ [(f, Gloss "" ["LOC"], 8) | f <- ["da", "de", "ta", "te"]]
          ++ [(f, Gloss "" ["ABL"], 8) | f <- ["dan", "den", "tan", "ten"]]
          ++ [(f, Gloss "" ["GEN"], 8) | f <- ["ın", "in", "un", "ün", "nın", "nin", "nun", "nün"]]
          ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ım", "im", "um", "üm"]]
          ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ma", "me"]]
          ++ [(f, Gloss "" ["FUT"], 8) | f <- ["yacak", "yecek", "acak", "ecek"]]
          ++ [(f, Gloss "" ["PROG"], 8) | f <- ["yor", "ıyor", "iyor", "uyor", "üyor"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dı", "di", "du", "dü", "tı", "ti", "tu", "tü"]]
          ++ [(f, Gloss "" ["1SG"], 9) | f <- ["m"]]
          ++ [(f, Gloss "" ["2SG"], 9) | f <- ["n"]]
          ++ [(f, Gloss "" ["1PL"], 9) | f <- ["k"]]
      )
