-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Render (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("render", renderDocument baseDoc == "китаплар  мин\nкитап-лар мин\nbook-PL   ?")
  , ("renderGloss joins the lexical part and the tags", renderGloss (Gloss "book" ["PL", "ACC"]) == "book.PL.ACC")
  , ("renderGloss omits an empty lexical part", renderGloss (Gloss "" ["PL", "ACC"]) == "PL.ACC")
  , ("renderGloss of an empty gloss is empty", renderGloss (Gloss "" []) == "")
  , ("renderSegment of a segment without tokens has empty lines", renderSegment (Segment (SegmentId 0) []) == "\n\n")
  , ("renderDocument separates segments with a blank line", renderDocument (Document 0 [Segment (SegmentId 0) [], Segment (SegmentId 1) []]) == "\n\n\n\n\n\n")
  , ("displayWidth counts ordinary characters as one column", displayWidth "abc кітап" == 9)
  , ("displayWidth counts a combining mark as no column", displayWidth "e\769" == 1)
  , ("displayWidth counts the zero-width non-joiner as no column", displayWidth "a\8204b" == 2)
  , ("displayWidth counts wide characters as two columns", displayWidth "\20013\25991" == 4)
  , ("displayWidth of the empty text is zero", displayWidth "" == 0)
  , ("render aligns a combining mark by columns", renderSegment (buildSegment emptyDocument [("e\769", ["e\769"]), ("ab", ["ab"])]) == "e\769 ab\ne\769 ab\n? ?")
  , ("render aligns wide characters by columns", renderSegment (buildSegment emptyDocument [("\20013\25991", ["a"]), ("x", ["x"])]) == "\20013\25991 x\na    x\n?    ?")
  ]
