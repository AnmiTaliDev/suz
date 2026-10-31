-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Persian
  ( persian
  ) where

import Suz.Language
import Suz.Model

persian :: Language
persian =
  withImportRules ".،!؟?" NoFolding $
    mkLanguage
      "Persian"
      ["1SG", "PL", "ACC", "IPFV"]
      ( [("من", Gloss "" ["1SG"], 10)]
          ++ [("کتاب\8204", Gloss "book" [], 10)]
          ++ [("خوان", Gloss "read" [], 10)]
          ++ [("ها", Gloss "" ["PL"], 9)]
          ++ [("را", Gloss "" ["ACC"], 10)]
          ++ [("می\8204", Gloss "" ["IPFV"], 9)]
          ++ [("م", Gloss "" ["1SG"], 9)]
      )
