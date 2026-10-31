-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Greek
  ( greek
  ) where

import Suz.Language
import Suz.Model

-- | A small Modern Greek weight table: the stems διαβάζ and βιβλί, the ending
-- ω for the first person singular present and the ending α for the nominative
-- and accusative plural. Importing splits the punctuation .,!? and
-- lowercases.
greek :: Language
greek =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Greek"
      ["1SG", "PRS", "PL", "NOM", "ACC"]
      ( [("διαβάζ", Gloss "read" [], 10)]
          ++ [("βιβλί", Gloss "book" [], 10)]
          ++ [("ω", Gloss "" ["1SG", "PRS"], 9)]
          ++ [("α", Gloss "" ["NOM", "ACC", "PL"], 9)]
      )
