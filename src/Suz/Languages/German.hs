-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.German
  ( german
  ) where

import Suz.Language
import Suz.Model

-- | A small German weight table: the pronoun ich, the stems les and büch, the
-- ending e for the first person singular present and the plural ending er.
-- Importing splits the punctuation .,!? and lowercases.
german :: Language
german =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "German"
      ["PL", "1SG", "PRS"]
      ( [("ich", Gloss "" ["1SG"], 10)]
          ++ [("les", Gloss "read" [], 10)]
          ++ [("büch", Gloss "book" [], 10)]
          ++ [("e", Gloss "" ["1SG", "PRS"], 9)]
          ++ [("er", Gloss "" ["PL"], 9)]
      )
