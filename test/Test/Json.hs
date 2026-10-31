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
  , ("json of a morph with UD fields", "{\"id\":2,\"form\":\"китап\",\"gloss\":{\"lexical\":\"book\",\"tags\":[]},\"ud\":{\"lemma\":\"kitap\",\"upos\":\"NOUN\",\"xpos\":\"\",\"feats\":\"Number=Plur\",\"head\":3,\"deprel\":\"obj\",\"deps\":\"\",\"misc\":\"SpaceAfter=No\"}}" `T.isInfixOf` renderJson udBase)
  , ("json writes the root head as a string", "\"head\":\"root\"" `T.isInfixOf` renderJson (udWith RootHead))
  , ("json writes a missing head as null", "\"head\":null" `T.isInfixOf` renderJson (udWith NoHead))
  , ("json leaves out ud for a morph without annotation", not ("\"ud\"" `T.isInfixOf` renderJson baseDoc))
  ]
  where
    surfaceDoc s = Document 100 [buildSegment emptyDocument [(s, [])]]
    udWith h = fst (must (apply (SetUd mA1 (Just udSample {udHead = h})) baseDoc))
