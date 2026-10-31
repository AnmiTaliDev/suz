-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.French
  ( french
  ) where

import Suz.Language
import Suz.Model

-- | A small French weight table: the pronoun nous, the article des, the stems
-- lis and livre, the ending ons for the first person plural present and the
-- plural ending s. Importing splits the punctuation .,!? and lowercases.
french :: Language
french =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "French"
      ["1PL", "PRS", "INDEF", "PL"]
      ( [("nous", Gloss "" ["1PL"], 10)]
          ++ [("lis", Gloss "read" [], 10)]
          ++ [("livre", Gloss "book" [], 10)]
          ++ [("ons", Gloss "" ["1PL", "PRS"], 9)]
          ++ [("des", Gloss "" ["INDEF", "PL"], 9)]
          ++ [("s", Gloss "" ["PL"], 9)]
      )
