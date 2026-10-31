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
  , ("codec reads a cleared annotation", decodeCommand "set-ud\t2\t0" == Right (SetUd (MorphId 2) Nothing))
  , ("codec rejects a bad UD flag", errorOf (decodeCommand "set-ud\t2\tx") == Just "bad UD flag")
  , ("codec rejects missing UD fields", errorOf (decodeCommand "set-ud\t2\t1\ta") == Just "unexpected end of line")
  , ("codec rejects a bad UD head", errorOf (decodeCommand "set-ud\t2\t1\t-\t-\t-\t-\tx\t-\t-\t-") == Just "bad head: x")
  ]
