-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Parser (checks) where

import Suz.Codec.Parser

checks :: [(String, Bool)]
checks =
  [ ("field takes the first field", runParser field ["a", "b"] == Right ("a", ["b"]))
  , ("field fails at the end of the line", runParser field [] == Left "unexpected end of line")
  , ("failP fails with its message", runParser (failP "no" :: Parser Int) ["a"] == Left "no")
  , ("intP reads an integer", runParser intP ["12", "x"] == Right (12, ["x"]))
  , ("intP reads a negative integer", runParser intP ["-3"] == Right (-3, []))
  , ("intP rejects other text", runParser intP ["x"] == Left "bad number: x")
  , ("wordP reads an identifier", runParser wordP ["7"] == Right (7, []))
  , ("wordP reads the largest identifier", runParser wordP ["18446744073709551615"] == Right (maxBound, []))
  , ("wordP rejects a negative number", runParser wordP ["-1"] == Left "bad identifier: -1")
  , ("wordP rejects a number that is too large", runParser wordP ["18446744073709551616"] == Left "bad identifier: 18446744073709551616")
  , ("counted reads a count and the items", runParser (counted field) ["2", "a", "b", "c"] == Right (["a", "b"], ["c"]))
  , ("counted accepts a count of zero", runParser (counted field) ["0", "a"] == Right ([], ["a"]))
  , ("counted rejects a negative count", runParser (counted field) ["-1"] == Left "negative count")
  , ("counted fails when items are missing", runParser (counted field) ["3", "a"] == Left "unexpected end of line")
  ]
