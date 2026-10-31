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
  , ("parseJson reads what renderJson wrote", fmap documentSegments (parseJson (renderJson baseDoc)) == Right (documentSegments baseDoc))
  , ("parseJson reads an annotated document", fmap documentSegments (parseJson (renderJson udBase)) == Right (documentSegments udBase))
  , ("parseJson reads a root head and a missing head", fmap documentSegments (parseJson (renderJson (udWith RootHead))) == Right (documentSegments (udWith RootHead)) && fmap documentSegments (parseJson (renderJson (udWith NoHead))) == Right (documentSegments (udWith NoHead)))
  , ("parseJson reads an empty document", parseJson "{\"segments\":[]}" == Right emptyDocument)
  , ("parseJson sets the next identifier past the largest", fmap documentNextId (parseJson (renderJson baseDoc)) == Right 6)
  , ("parseJson allows white space and any member order", parseJson " {\n \"segments\" : [ { \"tokens\" : [ ] , \"id\" : 7 } ]\n} " == Right (Document 8 [Segment (SegmentId 7) []]))
  , ("parseJson ignores unknown members", parseJson "{\"segments\":[],\"x\":[1,2.5e3,true,null,{}]}" == Right emptyDocument)
  , ("parseJson takes a missing gloss and ud as none", fmap (map (\m -> (morphGloss m, morphUd m)) . allMorphs) (parseJson (morphJson "{\"id\":2,\"form\":\"a\"}")) == Right [(Nothing, Nothing)])
  , ("parseJson takes missing ud columns as empty", fmap (map morphUd . allMorphs) (parseJson (morphJson "{\"id\":2,\"form\":\"a\",\"ud\":{\"upos\":\"X\"}}")) == Right [Just (UdFields "" "X" "" "" NoHead "" "" "")])
  , ("parseJson reads escapes", fmap (map morphForm . allMorphs) (parseJson (morphJson "{\"id\":2,\"form\":\"\\u043c\\u0438\\u043d\\/\\b\\f\\\"\"}")) == Right ["мин/\b\f\""])
  , ("parseJson reads a surrogate pair", fmap (map morphForm . allMorphs) (parseJson (morphJson "{\"id\":2,\"form\":\"\\ud83d\\ude00\"}")) == Right ["\128512"])
  , ("parseJson rejects a lone surrogate", isLeft (parseJson (morphJson "{\"id\":2,\"form\":\"\\ud83d\"}")) && isLeft (parseJson (morphJson "{\"id\":2,\"form\":\"\\ude00\"}")))
  , ("parseJson rejects a bad escape", isLeft (parseJson (morphJson "{\"id\":2,\"form\":\"\\x\"}")))
  , ("parseJson rejects a raw control character in a string", isLeft (parseJson (morphJson "{\"id\":2,\"form\":\"a\tb\"}")))
  , ("parseJson rejects text that is not JSON", isLeft (parseJson "") && isLeft (parseJson "{") && isLeft (parseJson "{\"segments\":[],}") && isLeft (parseJson "{\"segments\":[1,]}") && isLeft (parseJson "nul"))
  , ("parseJson rejects text after the value", isLeft (parseJson "{\"segments\":[]} x"))
  , ("parseJson reports the line of a syntax error", parseJson "{\n\"segments\": [\n]x}" == Left "line 3: expected a comma or a closing brace")
  , ("parseJson rejects numbers with a bad form", all (isLeft . parseJson . idJson) ["01", "1.", "-", "1e", ".5", "+1"])
  , ("parseJson rejects an identifier that is not a whole number", all (isLeft . parseJson . idJson) ["-1", "1.5", "1e2", "\"1\"", "null", "18446744073709551616", "999999999999999999999999"])
  , ("parseJson rejects the largest identifier", parseJson (idJson "18446744073709551615") == Left "identifier is too large")
  , ("parseJson rejects a duplicate identifier", parseJson "{\"segments\":[{\"id\":1,\"tokens\":[{\"id\":1,\"surface\":\"\",\"morphs\":[]}]}]}" == Left "duplicate identifier: 1")
  , ("parseJson names a missing member", parseJson "{}" == Left "document: missing member: segments" && parseJson "{\"segments\":[{\"tokens\":[]}]}" == Left "segments[0]: missing member: id")
  , ("parseJson names a member of the wrong type", parseJson "{\"segments\":{}}" == Left "segments: expected an array" && parseJson (morphJson "{\"id\":2,\"form\":3}") == Left "segments[0].tokens[0].morphs[0].form: expected a string" && parseJson "[]" == Left "document: expected an object")
  , ("parseJson rejects a bad head", isLeft (parseJson (morphJson "{\"id\":2,\"form\":\"a\",\"ud\":{\"head\":\"top\"}}")) && isLeft (parseJson (morphJson "{\"id\":2,\"form\":\"a\",\"ud\":{\"head\":true}}")))
  , ("parseJson rejects nesting that is too deep", isLeft (parseJson (T.replicate 200 "[" <> T.replicate 200 "]")))
  ]
  where
    isLeft = either (const True) (const False)
    allMorphs = concatMap tokenMorphs . concatMap segmentTokens . documentSegments
    morphJson m = "{\"segments\":[{\"id\":0,\"tokens\":[{\"id\":1,\"surface\":\"\",\"morphs\":[" <> m <> "]}]}]}"
    idJson n = "{\"segments\":[{\"id\":" <> n <> ",\"tokens\":[]}]}"
    surfaceDoc s = Document 100 [buildSegment emptyDocument [(s, [])]]
    udWith h = fst (must (apply (SetUd mA1 (Just udSample {udHead = h})) baseDoc))
