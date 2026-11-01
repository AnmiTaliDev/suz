-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Flex (checks) where

import Data.Text (Text)
import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ( "flex of a glossed document"
    , T.lines (renderFlex "tt" "en" baseDoc)
        ==
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<document version=\"2\">"
        , "  <interlinear-text>"
        , "    <item type=\"title\" lang=\"en\">Interlinear text</item>"
        , "    <paragraphs>"
        , "      <paragraph>"
        , "        <phrases>"
        , "          <phrase>"
        , "            <item type=\"segnum\" lang=\"en\">1</item>"
        , "            <words>"
        , "              <word>"
        , "                <item type=\"txt\" lang=\"tt\">китаплар</item>"
        , "                <morphemes>"
        , "                  <morph>"
        , "                    <item type=\"txt\" lang=\"tt\">китап</item>"
        , "                    <item type=\"gls\" lang=\"en\">book</item>"
        , "                  </morph>"
        , "                  <morph>"
        , "                    <item type=\"txt\" lang=\"tt\">лар</item>"
        , "                    <item type=\"gls\" lang=\"en\">PL</item>"
        , "                  </morph>"
        , "                </morphemes>"
        , "              </word>"
        , "              <word>"
        , "                <item type=\"txt\" lang=\"tt\">мин</item>"
        , "                <morphemes>"
        , "                  <morph>"
        , "                    <item type=\"txt\" lang=\"tt\">мин</item>"
        , "                  </morph>"
        , "                </morphemes>"
        , "              </word>"
        , "            </words>"
        , "          </phrase>"
        , "        </phrases>"
        , "      </paragraph>"
        , "    </paragraphs>"
        , "    <languages>"
        , "      <language lang=\"tt\" vernacular=\"true\"/>"
        , "      <language lang=\"en\"/>"
        , "    </languages>"
        , "  </interlinear-text>"
        , "</document>"
        ]
    )
  , ("flex ends with a line break", "</document>\n" `T.isSuffixOf` renderFlex "tt" "en" baseDoc)
  , ("flex of an empty document has no phrases", not ("<phrase>" `T.isInfixOf` renderFlex "tt" "en" emptyDocument) && "<paragraph>" `T.isInfixOf` renderFlex "tt" "en" emptyDocument)
  , ("flex skips a segment without tokens", not ("<phrase>" `T.isInfixOf` renderFlex "tt" "en" (Document 0 [Segment (SegmentId 0) []])))
  , ("flex numbers the phrases from 1", T.count "<item type=\"segnum\" lang=\"en\">" (renderFlex "tt" "en" twoSegmentDoc) == 2 && "lang=\"en\">2</item>" `T.isInfixOf` renderFlex "tt" "en" twoSegmentDoc)
  , ("flex joins a lexical part and tags with periods", glossItem (Gloss "book" ["PL", "ACC"]) == "book.PL.ACC")
  , ("flex writes tags alone", glossItem (Gloss "" ["1SG"]) == "1SG")
  , ("flex writes a lexical part alone", glossItem (Gloss "book" []) == "book")
  , ("flex leaves out the gloss of an unglossed morph", T.count "type=\"gls\"" (renderFlex "tt" "en" baseDoc) == 2)
  , ("flex writes a token of punctuation as a punct word", punctOf "." == ["<item type=\"punct\" lang=\"tt\">.</item>"] && not ("<morphemes>" `T.isInfixOf` snd (T.breakOn "type=\"punct\"" (renderFlex "tt" "en" (documentOf ".")))))
  , ("flex writes a token of symbols as a punct word", punctOf "?!\8212" == ["<item type=\"punct\" lang=\"tt\">?!\8212</item>"])
  , ("flex does not take a word with a letter for punctuation", punctOf "a." == [] && punctOf "" == [])
  , ("flex writes a word without morphs without morphemes", not ("<morphemes>" `T.isInfixOf` renderFlex "tt" "en" (Document 100 [buildSegment emptyDocument [("x", [])]])))
  , ("flex lists the two writing systems", T.count "<language " (renderFlex "tt" "en" baseDoc) == 2 && "<language lang=\"tt\" vernacular=\"true\"/>" `T.isInfixOf` renderFlex "tt" "en" baseDoc)
  , ("flex lists one writing system when both tags are equal", T.count "<language " (renderFlex "tt" "tt" baseDoc) == 1)
  , ("flex escapes markup characters", txtOf "a&b<c>\"d'" == ["<item type=\"txt\" lang=\"tt\">a&amp;b&lt;c&gt;&quot;d&apos;</item>"])
  , ("flex escapes the tags of the writing systems", "lang=\"a&quot;b\"" `T.isInfixOf` renderFlex "a\"b" "en" baseDoc)
  , ("parseFlex reads what renderFlex wrote", fmap documentSegments (parseFlex tinyLanguage (renderFlex "tt" "en" baseDoc)) == Right (documentSegments baseDoc))
  , ("parseFlex reads an empty document", parseFlex tinyLanguage (renderFlex "tt" "en" emptyDocument) == Right emptyDocument)
  , ("parseFlex sets the next identifier past the last", fmap documentNextId (parseFlex tinyLanguage (renderFlex "tt" "en" baseDoc)) == Right 6)
  , ("parseFlex makes a phrase of each segment", fmap (length . documentSegments) (parseFlex tinyLanguage (renderFlex "tt" "en" twoSegmentDoc)) == Right 2)
  , ("parseFlex splits a gloss at periods with the tags of the language", fmap glosses (parseFlex tinyLanguage (flex [phrase [word "x" [morph "x" (Just "book.PL")]]])) == Right [Just (Gloss "book" ["PL"])])
  , ("parseFlex takes a leading tag of the language as a tag", fmap glosses (parseFlex tinyLanguage (flex [phrase [word "x" [morph "x" (Just "PL.3SG")]]])) == Right [Just (Gloss "" ["PL", "3SG"])])
  , ("parseFlex leaves a morph without a gloss unglossed", fmap glosses (parseFlex tinyLanguage (flex [phrase [word "x" [morph "x" Nothing]]])) == Right [Nothing])
  , ("parseFlex reads a punct word as a token without morphs", fmap (map (\t -> (tokenSurface t, length (tokenMorphs t))) . concatMap segmentTokens . documentSegments) (parseFlex tinyLanguage (renderFlex "tt" "en" (documentOf "."))) == Right [(".", 0)])
  , ("parseFlex keeps a word without morphemes", fmap (map (length . tokenMorphs) . concatMap segmentTokens . documentSegments) (parseFlex tinyLanguage (flex [phrase [word "x" []]])) == Right [0])
  , ("parseFlex reads all paragraphs of all texts", fmap (length . documentSegments) (parseFlex tinyLanguage ("<document>" <> text [phrase [word "a" []]] <> text [phrase [word "b" []], phrase [word "c" []]] <> "</document>")) == Right 3)
  , ("parseFlex ignores free translations and word glosses", fmap formsOf (parseFlex tinyLanguage (flex ["<phrase><words><word><item type=\"txt\" lang=\"tt\">x</item><item type=\"gls\" lang=\"en\">w</item><morphemes><morph><item type=\"txt\" lang=\"tt\">x</item></morph></morphemes></word></words><item type=\"gls\" lang=\"en\">free</item></phrase>"])) == Right ["x"])
  , ("parseFlex takes the first form when there are several writing systems", fmap formsOf (parseFlex tinyLanguage (flex ["<phrase><words><word><item type=\"txt\" lang=\"tt\">x</item><morphemes><morph><item type=\"txt\" lang=\"tt\">y</item><item type=\"txt\" lang=\"tt-Latn\">z</item></morph></morphemes></word></words></phrase>"])) == Right ["y"])
  , ("parseFlex leaves out a word without a form", fmap (length . concatMap segmentTokens . documentSegments) (parseFlex tinyLanguage (flex ["<phrase><words><word><item type=\"gls\" lang=\"en\">w</item></word></words></phrase>"])) == Right 0)
  , ("parseFlex leaves out a morph without a form", fmap formsOf (parseFlex tinyLanguage (flex ["<phrase><words><word><item type=\"txt\" lang=\"tt\">x</item><morphemes><morph><item type=\"gls\" lang=\"en\">g</item></morph></morphemes></word></words></phrase>"])) == Right [])
  , ("parseFlex reads entities and CDATA", fmap formsOf (parseFlex tinyLanguage (flex [phrase [word "x" [morph "a&amp;b <![CDATA[<c>]]>" Nothing]]])) == Right ["a&b <c>"])
  , ("parseFlex rejects a file that is not a FLExText document", parseFlex tinyLanguage "<a/>" == Left "not a FLExText document")
  , ("parseFlex rejects text that is not XML", fmap (const ()) (parseFlex tinyLanguage "<document>") == Left "line 1: unclosed element: document")
  ]
  where
    flex ps = "<?xml version=\"1.0\"?><document version=\"2\">" <> text ps <> "</document>"
    text ps = "<interlinear-text><paragraphs><paragraph><phrases>" <> T.concat ps <> "</phrases></paragraph></paragraphs></interlinear-text>"
    phrase ws = "<phrase><words>" <> T.concat ws <> "</words></phrase>"
    word surface ms = "<word><item type=\"txt\" lang=\"tt\">" <> surface <> "</item>" <> (if null ms then "" else "<morphemes>" <> T.concat ms <> "</morphemes>") <> "</word>"
    morph form g = "<morph><item type=\"txt\" lang=\"tt\">" <> form <> "</item>" <> maybe "" (\x -> "<item type=\"gls\" lang=\"en\">" <> x <> "</item>") g <> "</morph>"
    glosses = map morphGloss . allMorphs
    formsOf = map morphForm . allMorphs
    allMorphs = concatMap tokenMorphs . concatMap segmentTokens . documentSegments
    documentOf s = Document 100 [buildSegment emptyDocument [(s, [s])]]
    textItems doc kind = [l | l <- map T.strip (T.lines (renderFlex "tt" "en" doc)), ("<item type=\"" <> kind <> "\"") `T.isPrefixOf` l]
    punctOf s = textItems (documentOf s) "punct"
    txtOf s = take 1 (textItems (Document 100 [buildSegment emptyDocument [(s, [])]]) "txt")
    glossItem :: Gloss -> Text
    glossItem g =
      let doc = fst (must (apply (SetGloss (MorphId 2) (Just g)) (Document 3 [buildSegment emptyDocument [("x", ["x"])]])))
          line = head' (textItems doc "gls")
       in fst (T.breakOn "</item>" (T.drop 1 (snd (T.breakOn ">" line))))
    head' = foldr const ""
