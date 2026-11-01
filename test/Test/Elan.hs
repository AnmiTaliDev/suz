-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Elan (checks) where

import Data.Text (Text)
import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ( "elan of a glossed document"
    , T.lines (renderElan baseDoc)
        ==
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<ANNOTATION_DOCUMENT AUTHOR=\"\" DATE=\"1970-01-01T00:00:00+00:00\" FORMAT=\"3.0\" VERSION=\"3.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.mpi.nl/tools/elan/EAFv3.0.xsd\">"
        , "    <HEADER MEDIA_FILE=\"\" TIME_UNITS=\"milliseconds\"/>"
        , "    <TIME_ORDER>"
        , "        <TIME_SLOT TIME_SLOT_ID=\"ts1\" TIME_VALUE=\"0\"/>"
        , "        <TIME_SLOT TIME_SLOT_ID=\"ts2\" TIME_VALUE=\"1000\"/>"
        , "    </TIME_ORDER>"
        , "    <TIER LINGUISTIC_TYPE_REF=\"segment\" TIER_ID=\"segment\">"
        , "        <ANNOTATION><ALIGNABLE_ANNOTATION ANNOTATION_ID=\"a0\" TIME_SLOT_REF1=\"ts1\" TIME_SLOT_REF2=\"ts2\"><ANNOTATION_VALUE>китаплар мин</ANNOTATION_VALUE></ALIGNABLE_ANNOTATION></ANNOTATION>"
        , "    </TIER>"
        , "    <TIER LINGUISTIC_TYPE_REF=\"word\" PARENT_REF=\"segment\" TIER_ID=\"word\">"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"a1\" ANNOTATION_REF=\"a0\"><ANNOTATION_VALUE>китаплар</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"a4\" ANNOTATION_REF=\"a0\" PREVIOUS_ANNOTATION=\"a1\"><ANNOTATION_VALUE>мин</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "    </TIER>"
        , "    <TIER LINGUISTIC_TYPE_REF=\"morph\" PARENT_REF=\"word\" TIER_ID=\"morph\">"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"a2\" ANNOTATION_REF=\"a1\"><ANNOTATION_VALUE>китап</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"a3\" ANNOTATION_REF=\"a1\" PREVIOUS_ANNOTATION=\"a2\"><ANNOTATION_VALUE>лар</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"a5\" ANNOTATION_REF=\"a4\"><ANNOTATION_VALUE>мин</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "    </TIER>"
        , "    <TIER LINGUISTIC_TYPE_REF=\"gloss\" PARENT_REF=\"morph\" TIER_ID=\"gloss\">"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"g2\" ANNOTATION_REF=\"a2\"><ANNOTATION_VALUE>book</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "        <ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"g3\" ANNOTATION_REF=\"a3\"><ANNOTATION_VALUE>PL</ANNOTATION_VALUE></REF_ANNOTATION></ANNOTATION>"
        , "    </TIER>"
        , "    <LINGUISTIC_TYPE GRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID=\"segment\" TIME_ALIGNABLE=\"true\"/>"
        , "    <LINGUISTIC_TYPE CONSTRAINTS=\"Symbolic_Subdivision\" GRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID=\"word\" TIME_ALIGNABLE=\"false\"/>"
        , "    <LINGUISTIC_TYPE CONSTRAINTS=\"Symbolic_Subdivision\" GRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID=\"morph\" TIME_ALIGNABLE=\"false\"/>"
        , "    <LINGUISTIC_TYPE CONSTRAINTS=\"Symbolic_Association\" GRAPHIC_REFERENCES=\"false\" LINGUISTIC_TYPE_ID=\"gloss\" TIME_ALIGNABLE=\"false\"/>"
        , "    <CONSTRAINT DESCRIPTION=\"Time subdivision of parent annotation's time interval, no time gaps allowed within this interval\" STEREOTYPE=\"Time_Subdivision\"/>"
        , "    <CONSTRAINT DESCRIPTION=\"Symbolic subdivision of a parent annotation. Annotations refering to the same parent are ordered\" STEREOTYPE=\"Symbolic_Subdivision\"/>"
        , "    <CONSTRAINT DESCRIPTION=\"1-1 association with a parent annotation\" STEREOTYPE=\"Symbolic_Association\"/>"
        , "    <CONSTRAINT DESCRIPTION=\"Time alignable annotations within the parent annotation's time interval, gaps are allowed\" STEREOTYPE=\"Included_In\"/>"
        , "</ANNOTATION_DOCUMENT>"
        ]
    )
  , ("elan ends with a line break", "</ANNOTATION_DOCUMENT>\n" `T.isSuffixOf` renderElan baseDoc)
  , ("elan of an empty document has no annotations", not ("<ANNOTATION>" `T.isInfixOf` renderElan emptyDocument) && "<TIER LINGUISTIC_TYPE_REF=\"gloss\"" `T.isInfixOf` renderElan emptyDocument)
  , ("elan skips a segment without tokens", not ("<ANNOTATION>" `T.isInfixOf` renderElan (Document 0 [Segment (SegmentId 0) []])))
  , ("elan gives each segment two time slots one second apart", T.count "<TIME_SLOT " (renderElan twoSegmentDoc) == 4 && "TIME_VALUE=\"2000\"" `T.isInfixOf` renderElan twoSegmentDoc)
  , ("elan joins a lexical part and tags with periods", glossValue (Gloss "book" ["PL", "ACC"]) == "book.PL.ACC")
  , ("elan writes tags alone", glossValue (Gloss "" ["1SG"]) == "1SG")
  , ("elan writes a lexical part alone", glossValue (Gloss "book" []) == "book")
  , ("elan leaves out the gloss of an unglossed morph", T.count "ANNOTATION_ID=\"g" (renderElan baseDoc) == 2)
  , ("elan points each morph at the one before it", "ANNOTATION_ID=\"a3\" ANNOTATION_REF=\"a1\" PREVIOUS_ANNOTATION=\"a2\"" `T.isInfixOf` renderElan baseDoc)
  , ("elan escapes markup characters", one "a&b<c>\"d'" == "<ANNOTATION_VALUE>a&amp;b&lt;c&gt;&quot;d&apos;</ANNOTATION_VALUE>")
  , ("elan turns a tab and a line break into spaces", one "a\tb\nc\rd" == "<ANNOTATION_VALUE>a b c d</ANNOTATION_VALUE>")
  , ("elan replaces other control characters", one "a\1b\31c\65535d" == "<ANNOTATION_VALUE>a\65533b\65533c\65533d</ANNOTATION_VALUE>")
  , ("parseElan reads what renderElan wrote", fmap documentSegments (parseElan tinyLanguage (renderElan baseDoc)) == Right (documentSegments baseDoc))
  , ("parseElan reads an empty document", parseElan tinyLanguage (renderElan emptyDocument) == Right emptyDocument)
  , ("parseElan sets the next identifier past the last", fmap documentNextId (parseElan tinyLanguage (renderElan baseDoc)) == Right 6)
  , ("parseElan splits a gloss at periods with the tags of the language", fmap glosses (parseElan tinyLanguage (eaf [segment "a0" "x"] [word "a1" "a0" Nothing "x"] [morph "a2" "a1" Nothing "x"] [gloss "g2" "a2" "book.PL"])) == Right [Just (Gloss "book" ["PL"])])
  , ("parseElan takes a leading tag of the language as a tag", fmap glosses (parseElan tinyLanguage (eaf [segment "a0" "x"] [word "a1" "a0" Nothing "x"] [morph "a2" "a1" Nothing "x"] [gloss "g2" "a2" "PL.3SG"])) == Right [Just (Gloss "" ["PL", "3SG"])])
  , ("parseElan leaves a morph without a gloss annotation unglossed", fmap glosses (parseElan tinyLanguage (eaf [segment "a0" "x"] [word "a1" "a0" Nothing "x"] [morph "a2" "a1" Nothing "x"] [])) == Right [Nothing])
  , ("parseElan splits the segment text when there is no word tier", fmap formsOf (parseElan tinyLanguage (eafTiers [segmentTier [segment "a0" "a  b\n c"]])) == Right ["a", "b", "c"])
  , ("parseElan gives a word one morph when there is no morph tier", fmap (map (map morphForm . tokenMorphs) . concatMap segmentTokens . documentSegments) (parseElan tinyLanguage (eafTiers [segmentTier [segment "a0" "x y"], wordsTier [word "a1" "a0" Nothing "x", word "a2" "a0" (Just "a1") "y"]])) == Right [["x"], ["y"]])
  , ("parseElan keeps a word without morphs", fmap (map (length . tokenMorphs) . concatMap segmentTokens . documentSegments) (parseElan tinyLanguage (eaf [segment "a0" "x y"] [word "a1" "a0" Nothing "x", word "a2" "a0" (Just "a1") "y"] [morph "a3" "a2" Nothing "y"] [])) == Right [0, 1])
  , ("parseElan finds the tiers by their place and not by name", fmap formsOf (parseElan tinyLanguage (renamed (renderElan baseDoc))) == Right ["китап", "лар", "мин"])
  , ("parseElan ignores other tiers", fmap formsOf (parseElan tinyLanguage (eafTiers [segmentTier [segment "a0" "x"], "<TIER LINGUISTIC_TYPE_REF=\"note\" TIER_ID=\"note\"><ANNOTATION><ALIGNABLE_ANNOTATION ANNOTATION_ID=\"n1\" TIME_SLOT_REF1=\"ts1\" TIME_SLOT_REF2=\"ts2\"><ANNOTATION_VALUE>z</ANNOTATION_VALUE></ALIGNABLE_ANNOTATION></ANNOTATION></TIER>"])) == Right ["x"])
  , ("parseElan reads entities and CDATA in values", fmap formsOf (parseElan tinyLanguage (eafTiers [segmentTier ["<ANNOTATION><ALIGNABLE_ANNOTATION ANNOTATION_ID=\"a0\" TIME_SLOT_REF1=\"ts1\" TIME_SLOT_REF2=\"ts2\"><ANNOTATION_VALUE>a&amp;b <![CDATA[<c>]]></ANNOTATION_VALUE></ALIGNABLE_ANNOTATION></ANNOTATION>"]])) == Right ["a&b", "<c>"])
  , ("parseElan rejects a file that is not an annotation document", parseElan tinyLanguage "<a/>" == Left "not an ELAN annotation document")
  , ("parseElan rejects text that is not XML", fmap (const ()) (parseElan tinyLanguage "<ANNOTATION_DOCUMENT>") == Left "line 1: unclosed element: ANNOTATION_DOCUMENT")
  , ("parseElan rejects a file without a tier", parseElan tinyLanguage "<ANNOTATION_DOCUMENT/>" == Left "no tier without a parent")
  , ("parseElan rejects a tier without an identifier", parseElan tinyLanguage "<ANNOTATION_DOCUMENT><TIER/></ANNOTATION_DOCUMENT>" == Left "a tier has no TIER_ID")
  , ("parseElan rejects an annotation without an identifier", parseElan tinyLanguage "<ANNOTATION_DOCUMENT><TIER TIER_ID=\"t\"><ANNOTATION><ALIGNABLE_ANNOTATION/></ANNOTATION></TIER></ANNOTATION_DOCUMENT>" == Left "tier t: an annotation has no ANNOTATION_ID")
  , ("parseElan rejects a reference annotation without a reference", parseElan tinyLanguage "<ANNOTATION_DOCUMENT><TIER TIER_ID=\"t\"><ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"a\"/></ANNOTATION></TIER></ANNOTATION_DOCUMENT>" == Left "tier t: annotation a has no ANNOTATION_REF")
  , ("elan keeps other characters", one "мин-китап. ;#\160" == "<ANNOTATION_VALUE>мин-китап. ;#\160</ANNOTATION_VALUE>")
  ]
  where
    glosses = map morphGloss . allMorphs
    formsOf = map morphForm . allMorphs
    allMorphs = concatMap tokenMorphs . concatMap segmentTokens . documentSegments
    renamed = T.replace "TIER_ID=\"morph\"" "TIER_ID=\"m\"" . T.replace "PARENT_REF=\"morph\"" "PARENT_REF=\"m\"" . T.replace "TIER_ID=\"word\"" "TIER_ID=\"w\"" . T.replace "PARENT_REF=\"word\"" "PARENT_REF=\"w\"" . T.replace "TIER_ID=\"segment\"" "TIER_ID=\"s\"" . T.replace "PARENT_REF=\"segment\"" "PARENT_REF=\"s\""
    annotation kind attrs v = "<ANNOTATION><" <> kind <> " " <> attrs <> "><ANNOTATION_VALUE>" <> v <> "</ANNOTATION_VALUE></" <> kind <> "></ANNOTATION>"
    segment i v = annotation "ALIGNABLE_ANNOTATION" ("ANNOTATION_ID=\"" <> i <> "\" TIME_SLOT_REF1=\"ts1\" TIME_SLOT_REF2=\"ts2\"") v
    refAnnotation i parent previous v = annotation "REF_ANNOTATION" ("ANNOTATION_ID=\"" <> i <> "\" ANNOTATION_REF=\"" <> parent <> "\"" <> maybe "" (\p -> " PREVIOUS_ANNOTATION=\"" <> p <> "\"") previous) v
    word i parent previous v = refAnnotation i parent previous v
    morph i parent previous v = refAnnotation i parent previous v
    gloss i parent v = refAnnotation i parent Nothing v
    tier name parent kind anns = "<TIER LINGUISTIC_TYPE_REF=\"" <> kind <> "\" " <> maybe "" (\p -> "PARENT_REF=\"" <> p <> "\" ") parent <> "TIER_ID=\"" <> name <> "\">" <> T.concat anns <> "</TIER>"
    segmentTier = tier "segment" Nothing "segment"
    wordsTier = tier "word" (Just "segment") "word"
    morphTier = tier "morph" (Just "word") "morph"
    glossTier = tier "gloss" (Just "morph") "gloss"
    eafTiers tiers =
      "<?xml version=\"1.0\"?><ANNOTATION_DOCUMENT><TIME_ORDER/>" <> T.concat tiers
        <> "<LINGUISTIC_TYPE LINGUISTIC_TYPE_ID=\"segment\"/><LINGUISTIC_TYPE CONSTRAINTS=\"Symbolic_Subdivision\" LINGUISTIC_TYPE_ID=\"word\"/><LINGUISTIC_TYPE CONSTRAINTS=\"Symbolic_Subdivision\" LINGUISTIC_TYPE_ID=\"morph\"/><LINGUISTIC_TYPE CONSTRAINTS=\"Symbolic_Association\" LINGUISTIC_TYPE_ID=\"gloss\"/></ANNOTATION_DOCUMENT>"
    eaf segs ws ms gs = eafTiers [segmentTier segs, wordsTier ws, morphTier ms, glossTier gs]
    one :: Text -> Text
    one s = fst (T.breakOn "</ANNOTATION_VALUE>" (wordTier (Document 100 [buildSegment emptyDocument [(s, [])]]))) <> "</ANNOTATION_VALUE>"
    wordTier doc = snd (T.breakOn "<ANNOTATION_VALUE>" (snd (T.breakOn "TIER_ID=\"word\"" (renderElan doc))))
    glossValue g =
      let doc = fst (must (apply (SetGloss (MorphId 2) (Just g)) (Document 3 [buildSegment emptyDocument [("x", ["x"])]])))
          out = snd (T.breakOn "TIER_ID=\"gloss\"" (renderElan doc))
       in fst (T.breakOn "</ANNOTATION_VALUE>" (T.drop (T.length "<ANNOTATION_VALUE>") (snd (T.breakOn "<ANNOTATION_VALUE>" out))))
