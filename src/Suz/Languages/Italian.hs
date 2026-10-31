-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Italian
  ( italian
  ) where

import Suz.Language
import Suz.Model

-- | A small Italian weight table: the pronoun io, the stems legg and libr,
-- the ending o for the first person singular present and the plural ending i.
-- Importing splits the punctuation .,!? and lowercases.
italian :: Language
italian =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Italian"
      ["1SG", "PRS", "PL"]
      ( [("io", Gloss "" ["1SG"], 10)]
          ++ [("legg", Gloss "read" [], 10)]
          ++ [("libr", Gloss "book" [], 10)]
          ++ [("o", Gloss "" ["1SG", "PRS"], 9)]
          ++ [("i", Gloss "" ["PL"], 9)]
      )
