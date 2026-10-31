-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Model (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("findSegment gives position and segment", findSegment (segmentId plainSegment) plainDoc == Just (0, plainSegment))
  , ("findSegment gives nothing for an unknown identifier", findSegment (SegmentId 999) plainDoc == Nothing)
  ]
