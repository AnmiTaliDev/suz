-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Html (checks) where

import Data.Text (Text)
import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ( "html of a glossed document"
    , body (renderHtml baseDoc)
        == [ "<li dir=\"auto\">"
           , word "китаплар" (form "китап" <> "-" <> form "лар") (gloss "book" <> "-" <> tagsOnly "pl")
           , word "мин" (form "мин") unglossed
           , "</li>"
           ]
    )
  , ("html is a complete page", let out = renderHtml baseDoc in "<!DOCTYPE html>\n" `T.isPrefixOf` out && "</html>\n" `T.isSuffixOf` out)
  , ("html declares the encoding and a title", let out = renderHtml baseDoc in "<meta charset=\"utf-8\">" `T.isInfixOf` out && "<title>Interlinear text</title>" `T.isInfixOf` out)
  , ("html carries its style sheet", let out = renderHtml baseDoc in "<style>\n" `T.isInfixOf` out && "\n</style>" `T.isInfixOf` out)
  , ("html of an empty document has an empty list", body (renderHtml emptyDocument) == [])
  , ("html skips a segment without tokens", body (renderHtml (Document 0 [Segment (SegmentId 0) []])) == [])
  , ("html gives each segment a list item", length (filter (== "<li dir=\"auto\">") (body (renderHtml twoSegmentDoc))) == 2)
  , ("html sets a lexical part and tags apart", inner (renderHtml (glossed (Gloss "book" ["PL", "ACC"]))) == word "x" (form "x") ("<span class=\"gloss\">book." <> tag "pl" <> "." <> tag "acc" <> "</span>"))
  , ("html writes tags alone", inner (renderHtml (glossed (Gloss "" ["1SG"]))) == word "x" (form "x") (tagsOnly "1sg"))
  , ("html writes a lexical part alone", inner (renderHtml (glossed (Gloss "book" []))) == word "x" (form "x") (gloss "book"))
  , ("html escapes markup characters", one "a&b<c>\"d'" ["x"] == word "a&amp;b&lt;c&gt;&quot;d&#39;" (form "x") unglossed)
  , ("html escapes the forms of morphs", one "x" ["<b>"] == word "x" (form "&lt;b&gt;") unglossed)
  , ("html turns a tab and a line break into spaces", one "a\tb\nc" ["x"] == word "a b c" (form "x") unglossed)
  , ("html joins morphs with hyphens", one "xy" ["x", "y"] == word "xy" (form "x" <> "-" <> form "y") (unglossed <> "-" <> unglossed))
  , ("html of a token without morphs has empty lines", one "x" [] == word "x" "" "")
  ]
  where
    body :: Text -> [Text]
    body out = takeWhile (/= "</ol>") (drop 1 (dropWhile (/= "<ol class=\"suz\">") (T.lines out)))
    inner :: Text -> Text
    inner out = case body out of
      [_, w, _] -> w
      _ -> ""
    one surface morphs = inner (renderHtml (Document 100 [buildSegment emptyDocument [(surface, morphs)]]))
    glossed g = fst (must (apply (SetGloss (MorphId 2) (Just g)) (Document 3 [buildSegment emptyDocument [("x", ["x"])]])))
    word :: Text -> Text -> Text -> Text
    word s f g = "<div class=\"word\"><span class=\"surface\">" <> s <> "</span><span class=\"forms\">" <> f <> "</span><span class=\"glosses\">" <> g <> "</span></div>"
    form :: Text -> Text
    form t = "<span class=\"form\">" <> t <> "</span>"
    gloss :: Text -> Text
    gloss t = "<span class=\"gloss\">" <> t <> "</span>"
    tag :: Text -> Text
    tag t = "<span class=\"tag\">" <> t <> "</span>"
    tagsOnly :: Text -> Text
    tagsOnly t = "<span class=\"gloss\">" <> tag t <> "</span>"
    unglossed :: Text
    unglossed = "<span class=\"gloss unglossed\">?</span>"
