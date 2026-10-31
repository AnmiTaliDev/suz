-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Latex (checks) where

import Data.Text (Text)
import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ( "latex of a glossed document"
    , renderLatex baseDoc == example "китаплар мин" "китап-лар мин" "book-\\textsc{pl} ?"
    )
  , ("latex of an empty document is empty", renderLatex emptyDocument == "")
  , ("latex skips a segment without tokens", renderLatex (Document 0 [Segment (SegmentId 0) []]) == "")
  , ("latex gives each segment an example in one environment", T.count "\\ex\n" (renderLatex twoSegmentDoc) == 2 && T.count "\\begin{exe}" (renderLatex twoSegmentDoc) == 1)
  , ("latex sets a lexical part and tags apart", glossed (Gloss "book" ["PL", "ACC"]) == example "x" "x" "book.\\textsc{pl.acc}")
  , ("latex sets tags alone in small capitals", glossed (Gloss "" ["1SG"]) == example "x" "x" "\\textsc{1sg}")
  , ("latex writes a lexical part alone", glossed (Gloss "book" []) == example "x" "x" "book")
  , ("latex shows an unglossed morph as a question mark", one "x" ["x"] == example "x" "x" "?")
  , ("latex escapes special characters", one "a_b&c%d$e#f{g}" ["x"] == example "a\\_b\\&c\\%d\\$e\\#f\\{g\\}" "x" "?")
  , ("latex escapes a backslash, a tilde and a caret", one "a\\b~c^d" ["x"] == example "a\\textbackslash{}b\\textasciitilde{}c\\textasciicircum{}d" "x" "?")
  , ("latex escapes angle brackets and a bar", one "<a|b>" ["x"] == example "\\textless{}a\\textbar{}b\\textgreater{}" "x" "?")
  , ("latex escapes the forms of morphs", one "x" ["a_b"] == example "x" "a\\_b" "?")
  , ("latex puts a word with a space in braces", one "a b" ["x"] == example "{a b}" "x" "?")
  , ("latex turns a tab and a line break into spaces", one "a\tb\nc" ["x"] == example "{a b c}" "x" "?")
  , ("latex puts an empty word in braces", one "x" [] == example "x" "{}" "{}")
  , ("latex joins morphs with hyphens", one "xy" ["x", "y"] == example "xy" "x-y" "?-?")
  ]
  where
    one word morphs = renderLatex (Document 100 [buildSegment emptyDocument [(word, morphs)]])
    glossed g = renderLatex (fst (must (apply (SetGloss (MorphId 2) (Just g)) (Document 3 [buildSegment emptyDocument [("x", ["x"])]]))))
    example :: Text -> Text -> Text -> Text
    example a b c =
      T.unlines
        [ "\\begin{exe}"
        , "\\ex"
        , "\\glll " <> a <> " \\\\"
        , "      " <> b <> " \\\\"
        , "      " <> c <> " \\\\"
        , "\\end{exe}"
        ]
