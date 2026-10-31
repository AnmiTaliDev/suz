-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Render (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("render", renderDocument baseDoc == "китаплар  мин\nкитап-лар мин\nbook-PL   ?")
  , ("renderGloss joins the lexical part and the tags", renderGloss (Gloss "book" ["PL", "ACC"]) == "book.PL.ACC")
  , ("renderGloss omits an empty lexical part", renderGloss (Gloss "" ["PL", "ACC"]) == "PL.ACC")
  , ("renderGloss of an empty gloss is empty", renderGloss (Gloss "" []) == "")
  ]
