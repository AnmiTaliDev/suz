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
  ]
