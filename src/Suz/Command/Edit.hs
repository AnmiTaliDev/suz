-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Command.Edit
  ( needSegment
  , needToken
  , needMorph
  , checkFresh
  , claimIds
  , insertAt
  , removeAt
  , replaceSegment
  , replaceToken
  , replaceMorph
  , releaseHeads
  , redirectHeads
  ) where

import Control.Monad (foldM_)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)
import Suz.Command.Types
import Suz.Model

needSegment :: SegmentId -> Document -> Either EditError (Int, Segment)
needSegment sid = maybe (Left (NoSuchSegment sid)) Right . findSegment sid

needToken :: TokenId -> Document -> Either EditError (Segment, Int, Token)
needToken tid = maybe (Left (NoSuchToken tid)) Right . findToken tid

needMorph :: MorphId -> Document -> Either EditError (Token, Int, Morph)
needMorph mid = maybe (Left (NoSuchMorph mid)) Right . findMorph mid

checkFresh :: Document -> [Word64] -> Either EditError ()
checkFresh doc = foldM_ claim (documentIds doc)
  where
    claim seen i
      | Set.member i seen = Left (DuplicateId i)
      | otherwise = Right (Set.insert i seen)

claimIds :: [Word64] -> Document -> Document
claimIds ids doc = doc {documentNextId = maximum (documentNextId doc : map (+ 1) ids)}

insertAt :: Int -> a -> [a] -> Either EditError [a]
insertAt i x xs
  | i < 0 || i > length xs = Left (IndexOutOfRange i)
  | otherwise = Right (take i xs ++ [x] ++ drop i xs)

removeAt :: Int -> [a] -> [a]
removeAt i xs = take i xs ++ drop (i + 1) xs

replaceSegment :: Segment -> Document -> Document
replaceSegment seg doc =
  doc
    { documentSegments =
        [if segmentId s == segmentId seg then seg else s | s <- documentSegments doc]
    }

mapTokens :: (Token -> Token) -> Document -> Document
mapTokens f doc =
  doc {documentSegments = [s {segmentTokens = map f (segmentTokens s)} | s <- documentSegments doc]}

replaceToken :: Token -> Document -> Document
replaceToken tok = mapTokens (\t -> if tokenId t == tokenId tok then tok else t)

replaceMorph :: Morph -> Document -> Document
replaceMorph morph = mapTokens (\t -> t {tokenMorphs = map swap (tokenMorphs t)})
  where
    swap m = if morphId m == morphId morph then morph else m

morphsOf :: Document -> [Morph]
morphsOf doc = [m | s <- documentSegments doc, t <- segmentTokens s, m <- tokenMorphs t]

releaseHeads :: Set MorphId -> Document -> (Document, [Command])
releaseHeads gone doc = foldl clear (doc, []) referrers
  where
    referrers = [(m, u) | m <- morphsOf doc, not (Set.member (morphId m) gone), Just u <- [morphUd m], HeadOf h <- [udHead u], Set.member h gone]
    clear (d, restores) (m, u) = (replaceMorph m {morphUd = Just u {udHead = NoHead}} d, SetUd (morphId m) (Just u) : restores)

redirectHeads :: MorphId -> MorphId -> Document -> (Document, [Command])
redirectHeads from to doc = foldl redirect (doc, []) referrers
  where
    referrers = [(m, u) | m <- morphsOf doc, morphId m /= from, Just u <- [morphUd m], udHead u == HeadOf from]
    redirect (d, restores) (m, u) =
      let hd = if morphId m == to then NoHead else HeadOf to
       in (replaceMorph m {morphUd = Just u {udHead = hd}} d, SetUd (morphId m) (Just u) : restores)
