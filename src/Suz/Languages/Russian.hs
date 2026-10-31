-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Russian
  ( russian
  ) where

import Suz.Language
import Suz.Model

russian :: Language
russian =
  withImportRules ".,!?" SimpleFolding $
    mkLanguage
      "Russian"
      ["PL", "ACC", "PRS", "1SG"]
      ( [("я", Gloss "" ["1SG"], 10)]
          ++ [("книг", Gloss "book" [], 10), ("чита", Gloss "read" [], 10)]
          ++ [("и", Gloss "" ["PL", "ACC"], 9)]
          ++ [("ю", Gloss "" ["1SG", "PRS"], 9)]
      )
