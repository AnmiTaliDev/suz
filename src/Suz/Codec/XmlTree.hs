-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec.XmlTree
  ( XmlElement (..)
  , XmlNode (..)
  , parseXml
  , attribute
  , childElements
  , elementText
  ) where

import Data.Char (chr, digitToInt, isDigit, isHexDigit, isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- | An XML element with its attributes in order and its content.
data XmlElement = XmlElement
  { xmlName :: Text
  , xmlAttributes :: [(Text, Text)]
  , xmlChildren :: [XmlNode]
  }
  deriving (Eq, Show)

data XmlNode = XmlText Text | XmlChild XmlElement
  deriving (Eq, Show)

-- | Value of the attribute with the given name.
attribute :: Text -> XmlElement -> Maybe Text
attribute name = lookup name . xmlAttributes

-- | Child elements with the given name, in order.
childElements :: Text -> XmlElement -> [XmlElement]
childElements name e = [c | XmlChild c <- xmlChildren e, xmlName c == name]

-- | All text inside the element, children included.
elementText :: XmlElement -> Text
elementText = T.concat . map node . xmlChildren
  where
    node (XmlText t) = t
    node (XmlChild c) = elementText c

-- A parser takes the rest of the input. A failure holds the rest of the input
-- at the point of the error and a description.
type Parser a = Text -> Either (Text, String) (a, Text)

maxDepth :: Int
maxDepth = 200

-- | Reads an XML document and returns its root element. The prolog, comments,
-- processing instructions and a document type declaration are skipped and
-- their entities are not defined, CDATA sections are text, and the five
-- predefined entities and character references are replaced. Names keep their
-- prefixes, there is no namespace handling. The error holds the line number
-- and a short description.
parseXml :: Text -> Either (Int, String) XmlElement
parseXml input = case start of
  Left (rest, msg) -> Left (lineAt rest, msg)
  Right (root, rest) -> case skipMisc rest of
    Left (r, msg) -> Left (lineAt r, msg)
    Right r
      | T.null r -> Right root
      | otherwise -> Left (lineAt r, "unexpected text after the root element")
  where
    start = do
      before <- skipMisc (T.dropWhile (== '\xFEFF') input)
      element 0 before
    lineAt rest = 1 + T.count "\n" (T.take (T.length input - T.length rest) input)

-- Skips white space, comments, processing instructions and a document type
-- declaration.
skipMisc :: Text -> Either (Text, String) Text
skipMisc t0
  | Just r <- T.stripPrefix "<?" t = pastEnd "?>" r >>= skipMisc
  | Just r <- T.stripPrefix "<!--" t = pastEnd "-->" r >>= skipMisc
  | Just r <- T.stripPrefix "<!DOCTYPE" t = doctype (0 :: Int) r >>= skipMisc
  | otherwise = Right t
  where
    t = T.dropWhile isSpace t0
    doctype depth r = case T.uncons r of
      Nothing -> Left (r, "unterminated document type declaration")
      Just ('>', r') | depth == 0 -> Right r'
      Just ('[', r') -> doctype (depth + 1) r'
      Just (']', r') -> doctype (depth - 1) r'
      Just (q, r') | q == '"' || q == '\'' -> case T.break (== q) r' of
        (_, rest) | T.null rest -> Left (rest, "unterminated document type declaration")
        (_, rest) -> doctype depth (T.drop 1 rest)
      Just (_, r') -> doctype depth r'

pastEnd :: Text -> Text -> Either (Text, String) Text
pastEnd marker t = case T.breakOn marker t of
  (_, rest)
    | T.null rest -> Left (t, "missing " ++ T.unpack marker)
    | otherwise -> Right (T.drop (T.length marker) rest)

element :: Int -> Parser XmlElement
element depth t0
  | depth > maxDepth = Left (t0, "nesting is too deep")
  | otherwise = case T.uncons t0 of
      Just ('<', r) | Just (c, _) <- T.uncons r, isNameStart c -> do
        let (name, r1) = T.span isNameChar r
        (attrs, r2, empty) <- attributes [] r1
        if empty
          then Right (XmlElement name (reverse attrs) [], r2)
          else do
            (children, r3) <- content depth name [] r2
            Right (XmlElement name (reverse attrs) children, r3)
      _ -> Left (t0, "expected an element")

isNameStart :: Char -> Bool
isNameStart c = not (isSpace c) && c `notElem` ("<>/=\"'&!?" :: String)

isNameChar :: Char -> Bool
isNameChar c = not (isSpace c) && c `notElem` ("<>/=\"'&" :: String)

attributes :: [(Text, Text)] -> Text -> Either (Text, String) ([(Text, Text)], Text, Bool)
attributes acc t0 =
  let t = T.dropWhile isSpace t0
   in case T.uncons t of
        Just ('>', r) -> Right (acc, r, False)
        Just ('/', r) -> case T.uncons r of
          Just ('>', r') -> Right (acc, r', True)
          _ -> Left (t, "expected >")
        Just (c, _) | isNameStart c -> do
          let (name, r1) = T.span isNameChar t
          r2 <- case T.uncons (T.dropWhile isSpace r1) of
            Just ('=', r') -> Right (T.dropWhile isSpace r')
            _ -> Left (r1, "expected = after an attribute name")
          (quote, r3) <- case T.uncons r2 of
            Just (q, r') | q == '"' || q == '\'' -> Right (q, r')
            _ -> Left (r2, "expected a quoted attribute value")
          let (raw, r4) = T.break (== quote) r3
          if T.null r4
            then Left (r3, "unterminated attribute value")
            else do
              value <- either (\msg -> Left (r3, msg)) Right (decode raw)
              if name `elem` map fst acc
                then Left (t, "repeated attribute: " ++ T.unpack name)
                else attributes ((name, value) : acc) (T.drop 1 r4)
        _ -> Left (t, "expected an attribute or >")

content :: Int -> Text -> [XmlNode] -> Parser [XmlNode]
content depth name acc t = case T.uncons t of
  Nothing -> Left (t, "unclosed element: " ++ T.unpack name)
  Just ('<', r)
    | Just r' <- T.stripPrefix "/" r -> do
        let (close, r1) = T.span isNameChar r'
        if close /= name
          then Left (t, "closing tag " ++ T.unpack close ++ " does not match " ++ T.unpack name)
          else case T.uncons (T.dropWhile isSpace r1) of
            Just ('>', r2) -> Right (reverse acc, r2)
            _ -> Left (r1, "expected >")
    | Just r' <- T.stripPrefix "!--" r -> pastEnd "-->" r' >>= content depth name acc
    | Just r' <- T.stripPrefix "![CDATA[" r -> do
        let (cdata, rest) = T.breakOn "]]>" r'
        if T.null rest
          then Left (r', "unterminated CDATA section")
          else content depth name (XmlText cdata : acc) (T.drop 3 rest)
    | Just r' <- T.stripPrefix "?" r -> pastEnd "?>" r' >>= content depth name acc
    | otherwise -> do
        (child, rest) <- element (depth + 1) t
        content depth name (XmlChild child : acc) rest
  Just _ -> do
    let (raw, rest) = T.break (== '<') t
    text <- either (\msg -> Left (t, msg)) Right (decode raw)
    content depth name (XmlText text : acc) rest

-- | Replaces the predefined entities and character references.
decode :: Text -> Either String Text
decode t = case T.splitOn "&" t of
  [] -> Right t
  first' : rest -> T.concat . (first' :) <$> traverse reference rest
  where
    reference s = case T.breakOn ";" s of
      (_, after) | T.null after -> Left "unterminated entity"
      (name, after) -> (<> T.drop 1 after) <$> entity name
    entity name = case T.unpack name of
      "amp" -> Right "&"
      "lt" -> Right "<"
      "gt" -> Right ">"
      "quot" -> Right "\""
      "apos" -> Right "'"
      '#' : 'x' : hex | not (null hex), all isHexDigit hex -> char (foldl (\n c -> n * 16 + toInteger (digitToInt c)) 0 hex)
      '#' : dec | not (null dec), all isDigit dec -> char (read dec)
      other -> Left ("unknown entity: " ++ other)
    char :: Integer -> Either String Text
    char n
      | n == 0x9 || n == 0xA || n == 0xD || (n >= 0x20 && n <= 0xD7FF) || (n >= 0xE000 && n <= 0xFFFD) || (n >= 0x10000 && n <= 0x10FFFF) = Right (T.singleton (chr (fromInteger n)))
      | otherwise = Left "character reference is not an XML character"

