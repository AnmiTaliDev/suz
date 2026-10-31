-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Edit (checks) where

import Fixtures
import Suz
import Suz.Command.Edit

checks :: [(String, Bool)]
checks =
  [ ("needSegment gives position and segment", needSegment (segmentId plainSegment) plainDoc == Right (0, plainSegment))
  , ("needSegment fails for an unknown identifier", needSegment (SegmentId 999) plainDoc == Left (NoSuchSegment (SegmentId 999)))
  , ("needToken gives segment, position and token", needToken (tokenId tokA) plainDoc == Right (plainSegment, 0, tokA))
  , ("needToken fails for an unknown identifier", needToken (TokenId 999) plainDoc == Left (NoSuchToken (TokenId 999)))
  , ("needMorph gives token, position and morph", needMorph mA1 plainDoc == Right (tokA, 0, tokenMorphs tokA !! 0))
  , ("needMorph fails for an unknown identifier", needMorph (MorphId 999) plainDoc == Left (NoSuchMorph (MorphId 999)))
  , ("checkFresh accepts unused identifiers", checkFresh plainDoc [nextFreeId plainDoc, nextFreeId plainDoc + 1] == Right ())
  , ("checkFresh rejects an identifier in use", checkFresh plainDoc [rawToken tokA] == Left (DuplicateId (rawToken tokA)))
  , ("checkFresh rejects a repeated identifier", checkFresh plainDoc [500, 500] == Left (DuplicateId 500))
  , ("claimIds raises the next free identifier", nextFreeId (claimIds [50, 7] plainDoc) == 51)
  , ("claimIds never lowers the next free identifier", nextFreeId (claimIds [0] plainDoc) == nextFreeId plainDoc)
  , ("insertAt inserts in the middle", insertAt 1 'x' "ab" == Right "axb")
  , ("insertAt inserts at the end", insertAt 2 'x' "ab" == Right "abx")
  , ("insertAt rejects a position past the end", insertAt 3 'x' "ab" == Left (IndexOutOfRange 3))
  , ("insertAt rejects a negative position", insertAt (-1) 'x' "ab" == Left (IndexOutOfRange (-1)))
  , ("removeAt removes one element", removeAt 1 "abc" == "ac")
  , ("removeAt ignores a position outside the list", removeAt 5 "abc" == "abc" && removeAt (-1) "abc" == "abc")
  , ("replaceSegment replaces by identifier", documentSegments (replaceSegment plainSegment {segmentTokens = []} plainDoc) == [plainSegment {segmentTokens = []}])
  , ("replaceToken replaces by identifier", map tokenSurface (concatMap segmentTokens (documentSegments (replaceToken tokB {tokenSurface = "Мин"} plainDoc))) == ["китаплар", "Мин"])
  , ("replaceMorph replaces by identifier", map morphForm (concatMap tokenMorphs (concatMap segmentTokens (documentSegments (replaceMorph ((tokenMorphs tokA !! 1) {morphForm = "ләр"}) plainDoc)))) == ["китап", "ләр", "мин"])
  , ("replace helpers ignore an unknown identifier", replaceMorph (Morph (MorphId 999) "x" Nothing Nothing) plainDoc == plainDoc && replaceToken (Token (TokenId 999) "x" []) plainDoc == plainDoc)
  ]
