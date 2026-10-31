-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Model
  ( SegmentId (..)
  , TokenId (..)
  , MorphId (..)
  , Gloss (..)
  , Morph (..)
  , Token (..)
  , Segment (..)
  , Document (..)
  , emptyDocument
  , nextFreeId
  , findSegment
  , findToken
  , findMorph
  , segmentNodeIds
  , tokenNodeIds
  , documentIds
  ) where

import Data.Maybe (listToMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Data.Word (Word64)

-- | Identifier of a segment. Segments, tokens and morphs share one numbering.
newtype SegmentId = SegmentId Word64 deriving (Eq, Ord, Show)

newtype TokenId = TokenId Word64 deriving (Eq, Ord, Show)

newtype MorphId = MorphId Word64 deriving (Eq, Ord, Show)

data Gloss = Gloss
  { glossLexical :: Text
  , glossTags :: [Text]
  }
  deriving (Eq, Ord, Show)

data Morph = Morph
  { morphId :: MorphId
  , morphForm :: Text
  , morphGloss :: Maybe Gloss
  }
  deriving (Eq, Show)

data Token = Token
  { tokenId :: TokenId
  , tokenSurface :: Text
  , tokenMorphs :: [Morph]
  }
  deriving (Eq, Show)

data Segment = Segment
  { segmentId :: SegmentId
  , segmentTokens :: [Token]
  }
  deriving (Eq, Show)

data Document = Document
  { documentNextId :: Word64
  , documentSegments :: [Segment]
  }
  deriving (Eq, Show)

-- | A document with no segments.
emptyDocument :: Document
emptyDocument = Document 0 []

-- | Smallest identifier that has never been used in the document. Segment,
-- token and morph identifiers share one numbering.
nextFreeId :: Document -> Word64
nextFreeId = documentNextId

-- | Position and value of the segment with the given identifier.
findSegment :: SegmentId -> Document -> Maybe (Int, Segment)
findSegment sid doc =
  listToMaybe
    [ (i, s)
    | (i, s) <- zip [0 ..] (documentSegments doc)
    , segmentId s == sid
    ]

-- | Owning segment, position in that segment and value of the token with the
-- given identifier.
findToken :: TokenId -> Document -> Maybe (Segment, Int, Token)
findToken tid doc =
  listToMaybe
    [ (s, i, t)
    | s <- documentSegments doc
    , (i, t) <- zip [0 ..] (segmentTokens s)
    , tokenId t == tid
    ]

-- | Owning token, position in that token and value of the morph with the
-- given identifier.
findMorph :: MorphId -> Document -> Maybe (Token, Int, Morph)
findMorph mid doc =
  listToMaybe
    [ (t, i, m)
    | s <- documentSegments doc
    , t <- segmentTokens s
    , (i, m) <- zip [0 ..] (tokenMorphs t)
    , morphId m == mid
    ]

-- | Raw identifiers of a segment and everything below it.
segmentNodeIds :: Segment -> [Word64]
segmentNodeIds (Segment (SegmentId n) ts) = n : concatMap tokenNodeIds ts

-- | Raw identifiers of a token and its morphs.
tokenNodeIds :: Token -> [Word64]
tokenNodeIds (Token (TokenId n) _ ms) = n : [m | Morph (MorphId m) _ _ <- ms]

-- | Raw identifiers of every node in the document.
documentIds :: Document -> Set Word64
documentIds = Set.fromList . concatMap segmentNodeIds . documentSegments
