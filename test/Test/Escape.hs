-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Escape (checks) where

import Suz

checks :: [(String, Bool)]
checks =
  [ ("escape doubles backslashes and escapes control characters", escape "a\\b\tc\nd\re" == "a\\\\b\\tc\\nd\\re")
  , ("escape keeps other text", escape "мин-китап. ;#" == "мин-китап. ;#")
  , ("unescape reads what escape writes", unescape "a\\\\b\\tc\\nd\\re" == Right "a\\b\tc\nd\re")
  ]
