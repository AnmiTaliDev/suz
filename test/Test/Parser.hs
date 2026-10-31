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
  ]
