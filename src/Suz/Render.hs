-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Render
  ( renderGloss
  , renderSegment
  , renderDocument
  , displayWidth
  ) where

import Data.Char (GeneralCategory (..), generalCategory, ord)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Model

-- | Lexical part and tags joined with periods. An empty lexical part is
-- omitted.
renderGloss :: Gloss -> Text
renderGloss (Gloss lexical tags) = T.intercalate "." (filter (not . T.null) [lexical] ++ tags)

-- | Three aligned lines: surface forms, morph forms joined with hyphens, and
-- glosses joined with hyphens. A morph without a gloss shows as a question
-- mark. Width is measured in terminal columns, see 'displayWidth'.
renderSegment :: Segment -> Text
renderSegment seg = T.intercalate "\n" (map row [0 .. 2])
  where
    cells = map tokenCells (segmentTokens seg)
    row k = T.stripEnd (T.unwords [padRight (cellWidth c) (pick k c) | c <- cells])
    pick k (a, b, c) = [a, b, c] !! k

-- | Number of terminal columns the text takes. Combining marks, enclosing
-- marks, format characters such as the zero-width non-joiner and control
-- characters take none, East Asian wide and fullwidth characters and emoji take
-- two, every other character one.
displayWidth :: Text -> Int
displayWidth = T.foldl' (\total c -> total + charWidth c) 0

charWidth :: Char -> Int
charWidth c
  | generalCategory c `elem` [NonSpacingMark, EnclosingMark, Format, Control] = 0
  | any (\(lo, hi) -> n >= lo && n <= hi) wideRanges = 2
  | otherwise = 1
  where
    n = ord c

wideRanges :: [(Int, Int)]
wideRanges =
  [ (0x1100, 0x115F)
  , (0x2E80, 0x303E)
  , (0x3041, 0x33FF)
  , (0x3400, 0x4DBF)
  , (0x4E00, 0x9FFF)
  , (0xA000, 0xA4CF)
  , (0xAC00, 0xD7A3)
  , (0xF900, 0xFAFF)
  , (0xFE30, 0xFE6F)
  , (0xFF00, 0xFF60)
  , (0xFFE0, 0xFFE6)
  , (0x1F300, 0x1F64F)
  , (0x1F900, 0x1F9FF)
  , (0x20000, 0x3FFFD)
  ]

padRight :: Int -> Text -> Text
padRight width t = t <> T.replicate (width - displayWidth t) " "

cellWidth :: (Text, Text, Text) -> Int
cellWidth (a, b, c) = maximum (map displayWidth [a, b, c])

-- | Segments rendered with 'renderSegment', separated by blank lines.
renderDocument :: Document -> Text
renderDocument = T.intercalate "\n\n" . map renderSegment . documentSegments

tokenCells :: Token -> (Text, Text, Text)
tokenCells t =
  ( tokenSurface t
  , T.intercalate "-" (map morphForm (tokenMorphs t))
  , T.intercalate "-" (map (maybe "?" renderGloss . morphGloss) (tokenMorphs t))
  )
