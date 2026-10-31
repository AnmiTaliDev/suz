-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Json (checks) where

import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("json of an empty document", renderJson emptyDocument == "{\"segments\":[]}")
  , ( "json of a glossed document"
    , renderJson baseDoc
        == "{\"segments\":[{\"id\":0,\"tokens\":[{\"id\":1,\"surface\":\"китаплар\",\"morphs\":[{\"id\":2,\"form\":\"китап\",\"gloss\":{\"lexical\":\"book\",\"tags\":[]}},{\"id\":3,\"form\":\"лар\",\"gloss\":{\"lexical\":\"\",\"tags\":[\"PL\"]}}]},{\"id\":4,\"surface\":\"мин\",\"morphs\":[{\"id\":5,\"form\":\"мин\",\"gloss\":null}]}]}]}"
    )
  , ("json escapes quotes and backslashes", "\"a\\\"b\\\\c\"" `T.isInfixOf` renderJson (surfaceDoc "a\"b\\c"))
  , ("json escapes line breaks and tabs", "\"a\\nb\\rc\\td\"" `T.isInfixOf` renderJson (surfaceDoc "a\nb\rc\td"))
  , ("json escapes other control characters", "\"a\\u0001b\\u001fc\"" `T.isInfixOf` renderJson (surfaceDoc "a\1b\31c"))
  , ("json keeps other characters", "\"мин-китап. ;#\160\"" `T.isInfixOf` renderJson (surfaceDoc "мин-китап. ;#\160"))
  ]
  where
    surfaceDoc s = Document 100 [buildSegment emptyDocument [(s, [])]]
