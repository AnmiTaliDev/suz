-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Build
  ( buildToken
  , buildSegment
  ) where

import Data.List (mapAccumL)
import Data.Text (Text)
import Data.Word (Word64)
import Suz.Model

-- | Token with the given surface form and one unglossed morph per form.
-- Identifiers start at 'nextFreeId' of the document. Build one node per
-- document state, since the identifiers are only claimed when the node is
-- inserted.
buildToken :: Document -> Text -> [Text] -> Token
buildToken doc surface forms = snd (tokenFrom (nextFreeId doc) (surface, forms))

-- | Segment from (surface, morph forms) pairs, with consecutive identifiers
-- starting at 'nextFreeId' of the document.
buildSegment :: Document -> [(Text, [Text])] -> Segment
buildSegment doc specs = Segment (SegmentId n) tokens
  where
    n = nextFreeId doc
    (_, tokens) = mapAccumL tokenFrom (n + 1) specs

tokenFrom :: Word64 -> (Text, [Text]) -> (Word64, Token)
tokenFrom n (surface, forms) =
  ( n + 1 + fromIntegral (length forms)
  , Token (TokenId n) surface (zipWith (\i f -> Morph (MorphId i) f Nothing) [n + 1 ..] forms)
  )
