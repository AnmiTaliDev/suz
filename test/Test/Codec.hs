-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Codec (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("codec round trip", all (\c -> decodeCommand (encodeCommand c) == Right c) commands)
  , ("codec rejects unknown command", errorOf (decodeCommand "bogus") /= Nothing)
  , ("codec rejects trailing fields", errorOf (decodeCommand "remove-token\t1\t2") /= Nothing)
  , ("codec rejects bad escape", errorOf (decodeCommand "set-form\t1\ta\\q") /= Nothing)
  ]
