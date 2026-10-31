-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Parser (checks) where

import Suz.Codec.Parser

checks :: [(String, Bool)]
checks =
  [ ("field takes the first field", runParser field ["a", "b"] == Right ("a", ["b"]))
  ]
