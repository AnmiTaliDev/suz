-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.LanguageBinary (checks) where

import qualified Data.ByteString as BS
import Data.Word (Word8)
import Fixtures
import Suz

v1Header :: [Word8]
v1Header = [0x53, 0x55, 0x5A, 0x4C, 1, 0]

checks :: [(String, Bool)]
checks =
  [ ("language binary round trip with context", decodeLanguage (encodeLanguage contextLanguage) == Right contextLanguage)
  , ( "language binary context layout"
    , encodeLanguage (withContext [("p", "x", Gloss "" [], 1)] (mkLanguage "a" [] []))
        == BS.pack (header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0] ++ [1, 0, 0, 0, 0x70] ++ [1, 0, 0, 0, 0x78] ++ [1, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0])
    )
  , ( "language binary reads version 1"
    , decodeLanguage (BS.pack (v1Header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0] ++ [1, 0, 0, 0, 0x78] ++ [1, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0]))
        == Right (mkLanguage "a" [] [("x", Gloss "" [], 1)])
    )
  , ("language binary version 1 has no context section", errorOf (decodeLanguage (BS.pack (v1Header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0]))) == Just "trailing data")
  , ("language binary round trip", decodeLanguage (encodeLanguage tinyLanguage) == Right tinyLanguage)
  , ("language binary round trip with escapes", decodeLanguage (encodeLanguage trickyLanguage) == Right trickyLanguage)
  , ("language binary is deterministic", fmap encodeLanguage (decodeLanguage trickyBytes) == Right trickyBytes)
  , ("language binary agrees with text", fmap renderLanguage (decodeLanguage trickyBytes) == Right (renderLanguage trickyLanguage))
  , ("language binary empty layout", encodeLanguage (mkLanguage "a" [] []) == BS.pack (header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]))
  , ( "language binary weight layout"
    , encodeLanguage (mkLanguage "a" [] [("x", Gloss "" [], 1)])
        == BS.pack (header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0] ++ [1, 0, 0, 0, 0x78] ++ [1, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0])
    )
  , ("language binary round trip with import rules", decodeLanguage (encodeLanguage ruled) == Right ruled)
  , ("language binary import rules layout", encodeLanguage ruled == BS.pack (take 4 header ++ [3, 0] ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0, 0x2E] ++ [1]))
  , ("language binary rejects a bad flag", errorOf (decodeLanguage (BS.pack (take 4 header ++ [3, 0] ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [2]))) == Just "bad flag")
  , ("language binary version 3 needs the rules", errorOf (decodeLanguage (BS.pack (take 4 header ++ [3, 0] ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0]))) == Just "unexpected end of data")
  , ("language binary rejects bad magic", errorOf (decodeLanguage (BS.pack (0x58 : drop 1 header ++ [1, 0, 0, 0, 0x61, 0, 0, 0, 0, 0, 0, 0, 0]))) == Just "bad magic")
  , ("language binary rejects other version", errorOf (decodeLanguage (BS.pack (take 4 header ++ [4, 0] ++ [1, 0, 0, 0, 0x61, 0, 0, 0, 0, 0, 0, 0, 0]))) == Just "unsupported version: 4")
  , ("language binary rejects truncated data", all (\n -> errorOf (decodeLanguage (BS.take n trickyBytes)) /= Nothing) [0 .. BS.length trickyBytes - 1])
  , ("language binary rejects trailing data", errorOf (decodeLanguage (trickyBytes <> BS.pack [0])) == Just "trailing data")
  , ("language binary rejects invalid UTF-8", errorOf (decodeLanguage (BS.pack (header ++ [1, 0, 0, 0, 0xFF, 0, 0, 0, 0, 0, 0, 0, 0]))) /= Nothing)
  , ("language binary rejects oversized length", errorOf (decodeLanguage (BS.pack (header ++ [0xFF, 0xFF, 0xFF, 0xFF]))) == Just "unexpected end of data")
  , ("language binary rejects non-finite weight", errorOf (decodeLanguage (encodeLanguage (mkLanguage "a" [] [("x", Gloss "" [], 0 / 0)]))) == Just "non-finite weight")
  ]
  where
    header = [0x53, 0x55, 0x5A, 0x4C, 2, 0]
    trickyBytes = encodeLanguage trickyLanguage
    ruled = withImportRules "." True (mkLanguage "a" [] [])
