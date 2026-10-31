-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Escape (checks) where

import Suz

checks :: [(String, Bool)]
checks =
  [ ("escape doubles backslashes and escapes control characters", escape "a\\b\tc\nd\re" == "a\\\\b\\tc\\nd\\re")
  ]
