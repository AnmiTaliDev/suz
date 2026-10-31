-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Latin
  ( latin
  ) where

import Suz.Language
import Suz.Model

-- | A small Latin weight table: the stems libr and leg, the ending os for the
-- accusative plural and the ending o for the first person singular present.
-- Importing splits the punctuation .,!? and lowercases.
latin :: Language
latin =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Latin"
      ["ACC", "PL", "1SG", "PRS"]
      ( [("libr", Gloss "book" [], 10)]
          ++ [("leg", Gloss "read" [], 10)]
          ++ [("os", Gloss "" ["ACC", "PL"], 9)]
          ++ [("o", Gloss "" ["1SG", "PRS"], 9)]
      )
