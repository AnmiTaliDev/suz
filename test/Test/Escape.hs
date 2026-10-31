-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Escape (checks) where

import Suz

checks :: [(String, Bool)]
checks =
  [ ("escape doubles backslashes and escapes control characters", escape "a\\b\tc\nd\re" == "a\\\\b\\tc\\nd\\re")
  , ("escape keeps other text", escape "мин-китап. ;#" == "мин-китап. ;#")
  , ("unescape reads what escape writes", unescape "a\\\\b\\tc\\nd\\re" == Right "a\\b\tc\nd\re")
  , ("unescape rejects an unknown escape", unescape "a\\xb" == Left "unknown escape")
  , ("unescape rejects a dangling backslash", unescape "ab\\" == Left "dangling escape")
  , ("encodeLexical writes an empty part as a hyphen", encodeLexical "" == "-")
  , ("encodeLexical escapes a literal hyphen", encodeLexical "-" == "\\-")
  , ("encodeLexical escapes other text", encodeLexical "a\tb" == "a\\tb")
  , ("decodeLexical reads a hyphen as an empty part", decodeLexical "-" == Right "")
  , ("decodeLexical reads an escaped hyphen", decodeLexical "\\-" == Right "-")
  ]
