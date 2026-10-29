-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Render
  ( renderGloss
  , renderSegment
  , renderDocument
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Suz.Model

-- | Lexical part and tags joined with periods. An empty lexical part is
-- omitted.
renderGloss :: Gloss -> Text
renderGloss (Gloss lexical tags) = T.intercalate "." (filter (not . T.null) [lexical] ++ tags)

-- | Three aligned lines: surface forms, morph forms joined with hyphens, and
-- glosses joined with hyphens. A morph without a gloss shows as a question
-- mark. Width is measured in code points.
renderSegment :: Segment -> Text
renderSegment seg = T.intercalate "\n" (map row [0 .. 2])
  where
    cells = map tokenCells (segmentTokens seg)
    row k = T.stripEnd (T.unwords [T.justifyLeft (width c) ' ' (pick k c) | c <- cells])
    pick k (a, b, c) = [a, b, c] !! k
    width (a, b, c) = maximum (map T.length [a, b, c])

-- | Segments rendered with 'renderSegment', separated by blank lines.
renderDocument :: Document -> Text
renderDocument = T.intercalate "\n\n" . map renderSegment . documentSegments

tokenCells :: Token -> (Text, Text, Text)
tokenCells t =
  ( tokenSurface t
  , T.intercalate "-" (map morphForm (tokenMorphs t))
  , T.intercalate "-" (map (maybe "?" renderGloss . morphGloss) (tokenMorphs t))
  )
