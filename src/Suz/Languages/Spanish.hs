-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Spanish
  ( spanish
  ) where

import Suz.Language
import Suz.Model

-- | A small Spanish weight table: the pronoun yo, the stems le and libr, the
-- endings o for the first person singular present and for the masculine class
-- vowel, and s for the plural. The ending o is the verb ending after le and
-- the class vowel after libr. Importing splits the punctuation .,!?¿¡ and
-- lowercases.
spanish :: Language
spanish =
  withImportRules ".,!?¿¡" SimpleFolding $
    withContext [("le", "o", Gloss "" ["1SG", "PRS"], 6), ("libr", "o", Gloss "" ["M"], 6)] $
    mkLanguage
      "Spanish"
      ["M", "PL", "1SG", "PRS"]
      ( [("yo", Gloss "" ["1SG"], 10)]
          ++ [("le", Gloss "read" [], 10)]
          ++ [("libr", Gloss "book" [], 10)]
          ++ [("o", Gloss "" ["1SG", "PRS"], 5)]
          ++ [("o", Gloss "" ["M"], 5)]
          ++ [("s", Gloss "" ["PL"], 9)]
      )
