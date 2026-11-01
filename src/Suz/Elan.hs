-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Elan
  ( renderElan
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec.Xml (escapeXml)
import Suz.Model

-- | An ELAN annotation file (EAF 3.0) of a document. There is no media file, so
-- the segments get made-up times, one second each in document order. The tiers
-- are @segment@ (time aligned, the surface forms of the tokens separated by
-- spaces), @word@ (the surface forms, a symbolic subdivision of a segment),
-- @morph@ (the forms, a symbolic subdivision of a word) and @gloss@ (a symbolic
-- association with a morph, left out for a morph without a gloss). A gloss is
-- its lexical part and its tags joined by periods. A segment without tokens is
-- left out. Annotation identifiers are @a@ and the identifier of the segment,
-- token or morph, @g@ and the identifier of the morph for a gloss. Text is
-- escaped with 'escapeXml'.
renderElan :: Document -> Text
renderElan doc =
  T.unlines $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<ANNOTATION_DOCUMENT AUTHOR=\"\" DATE=\"1970-01-01T00:00:00+00:00\" FORMAT=\"3.0\" VERSION=\"3.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xsi:noNamespaceSchemaLocation=\"http://www.mpi.nl/tools/elan/EAFv3.0.xsd\">"
    , "    <HEADER MEDIA_FILE=\"\" TIME_UNITS=\"milliseconds\"/>"
    , "    <TIME_ORDER>"
    ]
      ++ concat [["        " <> slot (2 * i) (i * 1000), "        " <> slot (2 * i + 1) (i * 1000 + 1000)] | (i, _) <- numbered]
      ++ [ "    </TIME_ORDER>"
         , "    <TIER LINGUISTIC_TYPE_REF=\"segment\" TIER_ID=\"segment\">"
         ]
      ++ [indent (segmentAnnotation i s) | (i, s) <- numbered]
      ++ [ "    </TIER>"
         , "    <TIER LINGUISTIC_TYPE_REF=\"word\" PARENT_REF=\"segment\" TIER_ID=\"word\">"
         ]
      ++ [indent a | s <- segs, a <- subdivision (segmentRef s) (map (\t -> (tokenRef t, tokenSurface t)) (segmentTokens s))]
      ++ [ "    </TIER>"
         , "    <TIER LINGUISTIC_TYPE_REF=\"morph\" PARENT_REF=\"word\" TIER_ID=\"morph\">"
         ]
      ++ [indent a | t <- tokens, a <- subdivision (tokenRef t) (map (\m -> (morphRef m, morphForm m)) (tokenMorphs t))]
      ++ [ "    </TIER>"
         , "    <TIER LINGUISTIC_TYPE_REF=\"gloss\" PARENT_REF=\"morph\" TIER_ID=\"gloss\">"
         ]
      ++ [indent (glossAnnotation m g) | t <- tokens, m <- tokenMorphs t, Just g <- [morphGloss m]]
      ++ [ "    </TIER>"
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
  where
    segs = filter (not . null . segmentTokens) (documentSegments doc)
    numbered = zip [0 :: Int ..] segs
    tokens = concatMap segmentTokens segs
    indent a = "        " <> a

slot :: Int -> Int -> Text
slot n ms = "<TIME_SLOT TIME_SLOT_ID=\"ts" <> number (n + 1) <> "\" TIME_VALUE=\"" <> number ms <> "\"/>"

segmentRef :: Segment -> Text
segmentRef s = "a" <> (let SegmentId n = segmentId s in number n)

tokenRef :: Token -> Text
tokenRef t = "a" <> (let TokenId n = tokenId t in number n)

morphRef :: Morph -> Text
morphRef m = "a" <> (let MorphId n = morphId m in number n)

segmentAnnotation :: Int -> Segment -> Text
segmentAnnotation i s =
  "<ANNOTATION><ALIGNABLE_ANNOTATION ANNOTATION_ID=\""
    <> segmentRef s
    <> "\" TIME_SLOT_REF1=\"ts"
    <> number (2 * i + 1)
    <> "\" TIME_SLOT_REF2=\"ts"
    <> number (2 * i + 2)
    <> "\">"
    <> value (T.unwords (map tokenSurface (segmentTokens s)))
    <> "</ALIGNABLE_ANNOTATION></ANNOTATION>"

-- | Annotations that subdivide the parent with the given reference, each
-- pointing at the one before it.
subdivision :: Text -> [(Text, Text)] -> [Text]
subdivision parent items = zipWith one (Nothing : map (Just . fst) items) items
  where
    one previous (ref, text) =
      "<ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\""
        <> ref
        <> "\" ANNOTATION_REF=\""
        <> parent
        <> "\""
        <> maybe "" (\p -> " PREVIOUS_ANNOTATION=\"" <> p <> "\"") previous
        <> ">"
        <> value text
        <> "</REF_ANNOTATION></ANNOTATION>"

glossAnnotation :: Morph -> Gloss -> Text
glossAnnotation m (Gloss lexical tags) =
  "<ANNOTATION><REF_ANNOTATION ANNOTATION_ID=\"g"
    <> T.drop 1 (morphRef m)
    <> "\" ANNOTATION_REF=\""
    <> morphRef m
    <> "\">"
    <> value (T.intercalate "." ([lexical | not (T.null lexical)] ++ tags))
    <> "</REF_ANNOTATION></ANNOTATION>"

value :: Text -> Text
value t = "<ANNOTATION_VALUE>" <> escapeXml t <> "</ANNOTATION_VALUE>"

number :: Show a => a -> Text
number = T.pack . show
