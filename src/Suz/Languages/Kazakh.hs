-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Kazakh
  ( kazakh
  ) where

import Suz.Language
import Suz.Model

-- | A small Kazakh weight table: the pronoun мен, the stems кітап, оқы and
-- үй, and endings for the plural, the accusative, dative, locative, ablative
-- and genitive cases, the first person singular possessive, negation, the
-- past tense and the first person singular, second person singular and first
-- person plural forms. The endings ды, ді, ты, ті are accusative after a noun
-- stem or a plural ending and past tense otherwise. Importing splits the
-- punctuation .,!? and lowercases.
kazakh :: Language
kazakh =
  withImportRules ".,!?" SimpleFolding $
    withContext [(p, f, Gloss "" ["ACC"], 6) | p <- plurals ++ nouns, f <- shared] $
      mkLanguage
        "Kazakh"
        ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
        ( [("мен", Gloss "" ["1SG"], 10)]
            ++ [("кітап", Gloss "book" [], 10), ("оқы", Gloss "read" [], 10), ("үй", Gloss "house" [], 10)]
            ++ [(f, Gloss "" ["PL"], 8) | f <- plurals]
            ++ [(f, Gloss "" ["ACC"], 5) | f <- shared]
            ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ны", "ні"]]
            ++ [(f, Gloss "" ["DAT"], 8) | f <- ["ға", "ге", "қа", "ке"]]
            ++ [(f, Gloss "" ["LOC"], 8) | f <- ["да", "де", "та", "те"]]
            ++ [(f, Gloss "" ["ABL"], 8) | f <- ["дан", "ден", "тан", "тен", "нан", "нен"]]
            ++ [(f, Gloss "" ["GEN"], 8) | f <- ["дың", "дің", "тың", "тің", "ның", "нің"]]
            ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ым", "ім"]]
            ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ма", "ме", "ба", "бе", "па", "пе"]]
            ++ [(f, Gloss "" ["PST"], 9) | f <- shared]
            ++ [(f, Gloss "" ["1SG"], 9) | f <- ["м"]]
            ++ [(f, Gloss "" ["2SG"], 9) | f <- ["ң"]]
            ++ [(f, Gloss "" ["1PL"], 9) | f <- ["қ", "к"]]
        )
  where
    plurals = ["лар", "лер", "дар", "дер", "тар", "тер"]
    nouns = ["кітап", "үй"]
    shared = ["ды", "ді", "ты", "ті"]
