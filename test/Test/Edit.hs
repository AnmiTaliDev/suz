-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Edit (checks) where

import Fixtures
import Suz
import Suz.Command.Edit

checks :: [(String, Bool)]
checks =
  [ ("needSegment gives position and segment", needSegment (segmentId plainSegment) plainDoc == Right (0, plainSegment))
  , ("needSegment fails for an unknown identifier", needSegment (SegmentId 999) plainDoc == Left (NoSuchSegment (SegmentId 999)))
  ]
