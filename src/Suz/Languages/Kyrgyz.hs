-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Kyrgyz
  ( kyrgyz
  ) where

import Suz.Language
import Suz.Model

-- | A small Kyrgyz weight table: the pronoun мен, the stems китеп, оку and
-- үй, and endings for the plural, the accusative, dative, locative, ablative
-- and genitive cases, the first person singular possessive, negation, the
-- past tense and the first person singular, second person singular and first
-- person plural forms. A vowel-final ending that can be accusative or past
-- tense is accusative after a noun stem or a plural ending. Importing splits
-- the punctuation .,!? and lowercases.
kyrgyz :: Language
kyrgyz =
  withImportRules ".,!?" SimpleFolding $
    withContext [(p, f, Gloss "" ["ACC"], 6) | p <- plurals ++ nouns, f <- shared] $
      mkLanguage
        "Kyrgyz"
        ["SG", "1SG", "PL", "ACC", "DAT", "LOC", "ABL", "GEN", "POSS", "NEG", "PST", "2SG", "1PL"]
        ( [("мен", Gloss "" ["1SG"], 10)]
            ++ [("китеп", Gloss "book" [], 10), ("оку", Gloss "read" [], 10), ("үй", Gloss "house" [], 10)]
            ++ [(f, Gloss "" ["PL"], 8) | f <- plurals]
            ++ [(f, Gloss "" ["ACC"], 5) | f <- shared]
            ++ [(f, Gloss "" ["ACC"], 9) | f <- ["ны", "ни", "ну", "нү"]]
            ++ [(f, Gloss "" ["DAT"], 8) | f <- ["га", "ге", "го", "гө", "ка", "ке", "ко", "кө"]]
            ++ [(f, Gloss "" ["LOC"], 8) | f <- ["да", "де", "до", "дө", "та", "те", "то", "тө"]]
            ++ [(f, Gloss "" ["ABL"], 8) | f <- ["дан", "ден", "дон", "дөн", "тан", "тен", "тон", "төн", "нан", "нен", "нон", "нөн"]]
            ++ [(f, Gloss "" ["GEN"], 8) | f <- ["дын", "дин", "дун", "дүн", "тын", "тин", "тун", "түн", "нын", "нин", "нун", "нүн"]]
            ++ [(f, Gloss "" ["1SG", "POSS"], 8) | f <- ["ым", "им", "ум", "үм"]]
            ++ [(f, Gloss "" ["NEG"], 8) | f <- ["ба", "бе", "бо", "бө", "па", "пе", "по", "пө", "ма", "ме", "мо", "мө"]]
            ++ [(f, Gloss "" ["PST"], 9) | f <- shared]
            ++ [(f, Gloss "" ["1SG"], 9) | f <- ["м"]]
            ++ [(f, Gloss "" ["2SG"], 9) | f <- ["ң"]]
            ++ [(f, Gloss "" ["1PL"], 9) | f <- ["к"]]
        )
  where
    plurals = ["лар", "лер", "лор", "лөр", "дар", "дер", "дор", "дөр", "тар", "тер", "тор", "төр"]
    nouns = ["китеп", "үй"]
    shared = ["ды", "ди", "ду", "дү", "ты", "ти", "ту", "тү"]
