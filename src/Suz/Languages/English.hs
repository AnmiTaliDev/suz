-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Languages.English
  ( english
  ) where

import Suz.Language
import Suz.Model

-- | A small English weight table: the words the, she, cat and walk, and the
-- endings s for the plural and the third person singular.
english :: Language
english =
  mkLanguage
    "English"
    ["SG", "PL", "3SG", "F", "DEF"]
    [ ("the", Gloss "" ["DEF"], 10)
    , ("she", Gloss "" ["3SG", "F"], 10)
    , ("cat", Gloss "cat" [], 10)
    , ("walk", Gloss "walk" [], 10)
    , ("s", Gloss "" ["PL"], 5.5)
    , ("s", Gloss "" ["3SG"], 5)
    ]
