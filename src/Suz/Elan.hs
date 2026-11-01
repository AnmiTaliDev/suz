-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Elan
  ( renderElan
  , parseElan
  , parseElanTier
  , elanTiers
  ) where

import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import Data.List (find, mapAccumL)
import Data.Maybe (isNothing)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec.Xml (escapeXml)
import Suz.Codec.XmlTree
import Suz.Interlinear (parseGloss, tokenFrom)
import Suz.Language
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

data Tier = Tier
  { tierId :: Text
  , tierParent :: Maybe Text
  , tierConstraint :: Maybe Text
  , tierAnnotations :: [Annotation]
  }

data Annotation = Annotation
  { annotationId :: Text
  , annotationRef :: Maybe Text
  , annotationValue :: Text
  }

-- | Reads an ELAN annotation file (EAF) with the structure that 'renderElan'
-- writes. The first tier without a parent holds the segments. A tier below it
-- that subdivides its annotations symbolically holds the words, one below that
-- the morphs, and a tier below the morphs that is a symbolic association holds
-- the glosses. The tiers are found by their place and constraint, not by
-- name, and other tiers are ignored. Without a word tier the words are the
-- text of a segment split at white space, and without a morph tier each word
-- has one morph with the word as its form. A gloss is split at periods: the
-- first part is a tag when the language lists it as one and the lexical part
-- otherwise, the rest are tags, and a morph without a gloss annotation is
-- unglossed. Annotations are used in the order of the file, times and
-- @PREVIOUS_ANNOTATION@ are not read. Identifiers are assigned in document
-- order from 0. The error holds a description, with the line for a syntax
-- error.
parseElan :: Language -> Text -> Either String Document
parseElan lang input = do
  tiers <- readTiers input
  top <- maybe (Left "no tier without a parent") Right (find (isNothing . tierParent) tiers)
  readDocument lang tiers top

-- | Like 'parseElan', with the segments taken from the tier without a parent
-- that has the given identifier. The tiers below it are found as for
-- 'parseElan'.
parseElanTier :: Language -> Text -> Text -> Either String Document
parseElanTier lang tier input = do
  tiers <- readTiers input
  top <- maybe (Left ("no tier without a parent named " ++ T.unpack tier)) Right (find (\t -> isNothing (tierParent t) && tierId t == tier) tiers)
  readDocument lang tiers top

-- | The identifiers of the tiers without a parent in an ELAN annotation file,
-- in the order of the file. 'parseElan' reads the first of them.
elanTiers :: Text -> Either String [Text]
elanTiers input = map tierId . filter (isNothing . tierParent) <$> readTiers input

readTiers :: Text -> Either String [Tier]
readTiers input = do
  root <- first (\(n, msg) -> "line " ++ show n ++ ": " ++ msg) (parseXml input)
  unless (xmlName root == "ANNOTATION_DOCUMENT") (Left "not an ELAN annotation document")
  let constraints = [(i, attribute "CONSTRAINTS" t) | t <- childElements "LINGUISTIC_TYPE" root, Just i <- [attribute "LINGUISTIC_TYPE_ID" t]]
  traverse (readTier constraints) (childElements "TIER" root)

readDocument :: Language -> [Tier] -> Tier -> Either String Document
readDocument lang tiers top = do
  let childOf parent kind = find (\t -> tierParent t == Just (tierId parent) && tierConstraint t == Just kind) tiers
      wordTier = childOf top "Symbolic_Subdivision"
      morphTier = wordTier >>= \w -> childOf w "Symbolic_Subdivision"
      glossTier = morphTier >>= \m -> childOf m "Symbolic_Association"
      groupsOf = maybe Map.empty (groupByRef . tierAnnotations)
      wordMap = groupsOf wordTier
      morphMap = groupsOf morphTier
      glossMap = groupsOf glossTier
      wordsOf a = case wordTier of
        Nothing -> [Annotation "" Nothing w | w <- T.words (annotationValue a)]
        Just _ -> Map.findWithDefault [] (annotationId a) wordMap
      formsOf w = case morphTier of
        Nothing -> [Annotation "" Nothing (annotationValue w)]
        Just _ -> Map.findWithDefault [] (annotationId w) morphMap
      glossOf m = case Map.lookup (annotationId m) glossMap of
        Just (g : _) -> first snd (parseGloss lang False (\_ t -> Right t) 0 (annotationValue g))
        _ -> Right Nothing
      token w = do
        let forms = formsOf w
        glosses <- traverse glossOf forms
        pure (annotationValue w, map annotationValue forms, glosses)
      segment n a = do
        triples <- traverse token (wordsOf a)
        let (end, tokens) = mapAccumL tokenFrom (n + 1) triples
        pure (end, Segment (SegmentId n) tokens)
      step (n, acc) a = (\(end, s) -> (end, s : acc)) <$> segment n a
  (next, segments) <- foldM step (0, []) (tierAnnotations top)
  pure (Document next (reverse segments))

groupByRef :: [Annotation] -> Map Text [Annotation]
groupByRef anns = Map.fromListWith (flip (++)) [(r, [a]) | a <- anns, Just r <- [annotationRef a]]

readTier :: [(Text, Maybe Text)] -> XmlElement -> Either String Tier
readTier constraints e = do
  tid <- maybe (Left "a tier has no TIER_ID") Right (attribute "TIER_ID" e)
  anns <- traverse (readAnnotation (T.unpack tid)) [a | c <- childElements "ANNOTATION" e, a <- xmlElements c]
  pure
    Tier
      { tierId = tid
      , tierParent = attribute "PARENT_REF" e
      , tierConstraint = attribute "LINGUISTIC_TYPE_REF" e >>= \r -> lookup r constraints >>= id
      , tierAnnotations = anns
      }

xmlElements :: XmlElement -> [XmlElement]
xmlElements e = [c | XmlChild c <- xmlChildren e]

readAnnotation :: String -> XmlElement -> Either String Annotation
readAnnotation tier e = do
  aid <- maybe (Left ("tier " ++ tier ++ ": an annotation has no ANNOTATION_ID")) Right (attribute "ANNOTATION_ID" e)
  let text = T.concat (map elementText (childElements "ANNOTATION_VALUE" e))
  case xmlName e of
    "ALIGNABLE_ANNOTATION" -> Right (Annotation aid Nothing text)
    "REF_ANNOTATION" -> case attribute "ANNOTATION_REF" e of
      Just r -> Right (Annotation aid (Just r) text)
      Nothing -> Left ("tier " ++ tier ++ ": annotation " ++ T.unpack aid ++ " has no ANNOTATION_REF")
    other -> Left ("tier " ++ tier ++ ": unexpected element " ++ T.unpack other)
