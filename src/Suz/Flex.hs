-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Flex
  ( renderFlex
  ) where

import Data.Char (isPunctuation, isSymbol)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec.Xml (escapeXml)
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
