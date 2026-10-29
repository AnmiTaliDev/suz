-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Validate
  ( Violation (..)
  , validateDocument
  ) where

import qualified Data.Set as Set
import Data.Text (Text)
import Suz.Language
import Suz.Model

data Violation = Violation
  { violationMorph :: MorphId
  , violationTags :: [Text]
  }
  deriving (Eq, Show)

-- | Glossed morphs whose gloss has tags the language does not declare, in
-- document order. Each violation lists the unknown tags in gloss order.
-- Morphs without a gloss are skipped.
validateDocument :: Language -> Document -> [Violation]
validateDocument lang doc =
  [ Violation (morphId m) unknown
  | s <- documentSegments doc
  , t <- segmentTokens s
  , m <- tokenMorphs t
  , Just g <- [morphGloss m]
  , let unknown = filter (`Set.notMember` languageTags lang) (glossTags g)
  , not (null unknown)
  ]
