-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.Persian
  ( persian
  ) where

import Suz.Language
import Suz.Model

-- | A small Persian weight table: the pronoun من, the stems کتاب and خوان,
-- the plural ending ها, the object marker را, the imperfective prefix می and
-- the first person singular ending م. The zero-width non-joiner that Persian
-- writes between morphs stays at the end of the form before it. Importing
-- splits the punctuation .،!؟? and keeps the case.
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
