-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.XmlTree (checks) where

import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec.XmlTree

checks :: [(String, Bool)]
checks =
  [ ("xml reads an element with attributes and text", parseXml "<a x=\"1\" y='2'>t</a>" == Right (XmlElement "a" [("x", "1"), ("y", "2")] [XmlText "t"]))
  , ("xml reads an empty element", parseXml "<a/>" == Right (XmlElement "a" [] []) && parseXml "<a x=\"1\" />" == Right (XmlElement "a" [("x", "1")] []))
  , ("xml reads children in order", fmap (map nodeName . xmlChildren) (parseXml "<a><b/>t<c></c></a>") == Right ["b", "#text", "c"])
  , ("xml replaces the predefined entities", fmap elementText (parseXml "<a>&amp;&lt;&gt;&quot;&apos;</a>") == Right "&<>\"'")
  , ("xml replaces character references", fmap elementText (parseXml "<a>&#65;&#x42;&#x1F600;</a>") == Right "AB\128512")
  , ("xml replaces entities in attribute values", fmap (attribute "x") (parseXml "<a x=\"&lt;&#x20;&amp;\"/>") == Right (Just "< &"))
  , ("xml rejects an unknown entity", isLeft (parseXml "<a>&nbsp;</a>") && isLeft (parseXml "<a>&amp</a>"))
  , ("xml rejects a character reference that is not an XML character", isLeft (parseXml "<a>&#0;</a>") && isLeft (parseXml "<a>&#xD800;</a>") && isLeft (parseXml "<a>&#x110000;</a>") && isLeft (parseXml "<a>&#;</a>"))
  , ("xml reads a CDATA section as text", fmap elementText (parseXml "<a>x<![CDATA[<&>]]>y</a>") == Right "x<&>y")
  , ("xml skips comments and processing instructions", fmap elementText (parseXml "<?xml version=\"1.0\"?><!-- c --><a>x<!-- <b/> -->y<?pi z?></a><!-- end -->") == Right "xy")
  , ("xml skips a document type declaration", fmap xmlName (parseXml "<!DOCTYPE a [<!ENTITY e \"v\"> <!-- > -->]><a/>") == Right "a")
  , ("xml does not define entities of a document type declaration", isLeft (parseXml "<!DOCTYPE a [<!ENTITY e \"v\">]><a>&e;</a>"))
  , ("xml skips a byte order mark and white space", fmap xmlName (parseXml "\xFEFF \n<a/>\n ") == Right "a")
  , ("xml keeps white space in text", fmap elementText (parseXml "<a> x\n y </a>") == Right " x\n y ")
  , ("xml keeps names with prefixes", fmap (map fst . xmlAttributes) (parseXml "<a xsi:b=\"1\" xmlns:xsi=\"u\"/>") == Right ["xsi:b", "xmlns:xsi"])
  , ("xml finds attributes and child elements", fmap (\e -> (attribute "x" e, map xmlName (childElements "b" e))) (parseXml "<a x=\"1\"><b/><c/><b/></a>") == Right (Just "1", ["b", "b"]))
  , ("xml rejects a closing tag that does not match", isLeft (parseXml "<a><b></a></b>") && isLeft (parseXml "<a></b>"))
  , ("xml rejects an element that is not closed", isLeft (parseXml "<a>") && isLeft (parseXml "<a><b/>") && isLeft (parseXml "<a"))
  , ("xml rejects text that is not XML", isLeft (parseXml "") && isLeft (parseXml "text") && isLeft (parseXml "<>") && isLeft (parseXml "<a x=1/>") && isLeft (parseXml "<a x=\"1/>") && isLeft (parseXml "<a x/>"))
  , ("xml rejects a repeated attribute", isLeft (parseXml "<a x=\"1\" x=\"2\"/>"))
  , ("xml rejects text after the root element", isLeft (parseXml "<a/>x") && isLeft (parseXml "<a/><b/>"))
  , ("xml rejects an unterminated comment, instruction and CDATA section", isLeft (parseXml "<a><!-- x</a>") && isLeft (parseXml "<a><?x</a>") && isLeft (parseXml "<a><![CDATA[x</a>"))
  , ("xml reports the line of an error", parseXml "<a>\n<b>\n</c>\n</a>" == Left (3, "closing tag c does not match b"))
  , ("xml rejects nesting that is too deep", isLeft (parseXml (T.replicate 300 "<a>" <> T.replicate 300 "</a>")))
  , ("xml accepts nesting of moderate depth", isRight (parseXml (T.replicate 150 "<a>" <> T.replicate 150 "</a>")))
  ]
  where
    nodeName :: XmlNode -> Text
    nodeName (XmlText _) = "#text"
    nodeName (XmlChild e) = xmlName e
    isLeft = either (const True) (const False)
    isRight = either (const False) (const True)
