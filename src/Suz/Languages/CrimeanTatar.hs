-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.CrimeanTatar
  ( crimeanTatar
  ) where

import Suz.Language
import Suz.Model

crimeanTatar :: Language
crimeanTatar =
  withImportRules ".,!?" TurkicFolding $
    mkLanguage
      "Crimean Tatar"
      ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
      ( [("men", Gloss "" ["1SG"], 10)]
          ++ [("kitap", Gloss "book" [], 10), ("oqu", Gloss "read" [], 10), ("ev", Gloss "house" [], 10)]
          ++ [(f, Gloss "" ["PL"], 8) | f <- ["lar", "ler"]]
          ++ [(f, Gloss "" ["ACC"], 9) | f <- ["nı", "ni"]]
          ++ [(f, Gloss "" ["DAT"], 8) | f <- ["ğa", "ge", "qa", "ke"]]
          ++ [(f, Gloss "" ["LOC"], 8) | f <- ["da", "de", "ta", "te"]]
          ++ [(f, Gloss "" ["ABL"], 8) | f <- ["dan", "den", "tan", "ten"]]
          ++ [(f, Gloss "" ["GEN"], 8) | f <- ["nıñ", "niñ"]]
          ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ım", "im"]]
          ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ma", "me"]]
          ++ [(f, Gloss "" ["PST"], 9) | f <- ["dı", "di", "du", "dü", "tı", "ti", "tu", "tü"]]
          ++ [(f, Gloss "" ["1SG"], 9) | f <- ["m"]]
          ++ [(f, Gloss "" ["2SG"], 9) | f <- ["ñ"]]
          ++ [(f, Gloss "" ["1PL"], 9) | f <- ["q", "k"]]
      )
