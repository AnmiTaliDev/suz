-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Spanish
  ( spanish
  ) where

import Suz.Language
import Suz.Model

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
