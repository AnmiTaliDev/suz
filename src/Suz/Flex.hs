-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Flex
  ( renderFlex
  , parseFlex
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, unless)
import Data.Bifunctor (first)
import Data.Char (isPunctuation, isSymbol)
import Data.List (mapAccumL)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec.Xml (escapeXml)
import Suz.Codec.XmlTree
import Suz.Interlinear (parseGloss, tokenFrom)
import Suz.Language
import Suz.Model

-- | A FLExText file (@.flextext@) of a document, as read by the FieldWorks
-- import of interlinear texts. The document is one text with one paragraph.
-- Every segment that has tokens is a phrase with its number in a @segnum@
-- item, every token a word with its surface form in a @txt@ item and its
-- morphs in @morphemes@, each with a @txt@ item for the form and a @gls@ item
-- for the gloss, left out for a morph without one. A token that has only
-- punctuation or symbols is a @punct@ word without morphs. A gloss is its
-- lexical part and its tags joined by periods. The first argument is the tag
-- of the writing system of the text, the second the one of the glosses and of
-- the title, which is not taken from the document. Text is escaped with
-- 'escapeXml'.
renderFlex :: Text -> Text -> Document -> Text
renderFlex vernacular analysis doc =
  T.unlines $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<document version=\"2\">"
    , "  <interlinear-text>"
    , "    " <> item "title" analysis "Interlinear text"
    , "    <paragraphs>"
    , "      <paragraph>"
    , "        <phrases>"
    ]
      ++ concat (zipWith phrase [1 :: Int ..] (filter (not . null . segmentTokens) (documentSegments doc)))
      ++ [ "        </phrases>"
         , "      </paragraph>"
         , "    </paragraphs>"
         , "    <languages>"
         , "      <language lang=\"" <> escapeXml vernacular <> "\" vernacular=\"true\"/>"
         ]
      ++ ["      <language lang=\"" <> escapeXml analysis <> "\"/>" | analysis /= vernacular]
      ++ [ "    </languages>"
         , "  </interlinear-text>"
         , "</document>"
         ]
  where
    phrase n seg =
      [ "          <phrase>"
      , "            " <> item "segnum" analysis (T.pack (show n))
      , "            <words>"
      ]
        ++ concatMap word (segmentTokens seg)
        ++ [ "            </words>"
           , "          </phrase>"
           ]
    word t
      | isPunctuationOnly (tokenSurface t) =
          [ "              <word>"
          , "                " <> item "punct" vernacular (tokenSurface t)
          , "              </word>"
          ]
      | otherwise =
          ["              <word>", "                " <> item "txt" vernacular (tokenSurface t)]
            ++ ( if null (tokenMorphs t)
                   then []
                   else ["                <morphemes>"] ++ concatMap morph (tokenMorphs t) ++ ["                </morphemes>"]
               )
            ++ ["              </word>"]
    morph m =
      ["                  <morph>", "                    " <> item "txt" vernacular (morphForm m)]
        ++ ["                    " <> item "gls" analysis (glossText g) | Just g <- [morphGloss m]]
        ++ ["                  </morph>"]

item :: Text -> Text -> Text -> Text
item kind lang text = "<item type=\"" <> kind <> "\" lang=\"" <> escapeXml lang <> "\">" <> escapeXml text <> "</item>"

glossText :: Gloss -> Text
glossText (Gloss lexical tags) = T.intercalate "." ([lexical | not (T.null lexical)] ++ tags)

isPunctuationOnly :: Text -> Bool
isPunctuationOnly t = not (T.null t) && T.all (\c -> isPunctuation c || isSymbol c) t

-- | Reads a FLExText file with the structure that 'renderFlex' writes. Every
-- @phrase@ of every paragraph of every text is a segment and every @word@ in
-- its @words@ a token. The surface form of a token is the first @txt@ item of
-- the word, or its @punct@ item when there is none, and a word with neither is
-- left out. The morphs are the @morph@ elements of @morphemes@, with the first
-- @txt@ item as the form and the first @gls@ item as the gloss; a morph
-- without a form is left out and one without a gloss is unglossed. A gloss is
-- split at periods: the first part is a tag when the language lists it as one
-- and the lexical part otherwise, the rest are tags. Other items, such as
-- free translations, and the writing system tags are ignored. Identifiers are
-- assigned in document order from 0. The error holds a description, with the
-- line for a syntax error.
parseFlex :: Language -> Text -> Either String Document
parseFlex lang input = do
  root <- first (\(n, msg) -> "line " ++ show n ++ ": " ++ msg) (parseXml input)
  unless (xmlName root == "document") (Left "not a FLExText document")
  let phrases = [ph | t <- childElements "interlinear-text" root, ps <- childElements "paragraphs" t, p <- childElements "paragraph" ps, phs <- childElements "phrases" p, ph <- childElements "phrase" phs]
      wordsOf ph = [w | ws <- childElements "words" ph, w <- childElements "word" ws]
      morphsOf w = [m | ms <- childElements "morphemes" w, m <- childElements "morph" ms]
      glossOf m = maybe (Right Nothing) (first snd . parseGloss lang False (\_ t -> Right t) 0) (itemText "gls" m)
      token w = do
        let morphs = [(f, m) | m <- morphsOf w, Just f <- [itemText "txt" m]]
        glosses <- traverse (glossOf . snd) morphs
        pure (map fst morphs, glosses)
      segment n ph = do
        triples <- sequence [(\(forms, glosses) -> (surface, forms, glosses)) <$> token w | w <- wordsOf ph, Just surface <- [itemText "txt" w <|> itemText "punct" w]]
        let (end, tokens) = mapAccumL tokenFrom (n + 1) triples
        pure (end, Segment (SegmentId n) tokens)
      step (n, acc) ph = (\(end, s) -> (end, s : acc)) <$> segment n ph
  (next, segments) <- foldM step (0, []) phrases
  pure (Document next (reverse segments))

itemText :: Text -> XmlElement -> Maybe Text
itemText kind e = listToMaybe (filter (not . T.null) [elementText i | i <- childElements "item" e, attribute "type" i == Just kind])
