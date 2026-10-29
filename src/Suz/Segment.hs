-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Segment
  ( segmentForm
  , suggestSegmentation
  ) where

import Data.List (mapAccumL, minimumBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Command
import Suz.Language
import Suz.Model

-- | Splits a form into forms known to the language. Only forms with a positive
-- total weight count as known. Among all splits the one with the fewest
-- pieces wins, then the one with the largest sum of weights, then the one
-- whose last piece is longest. Returns Nothing for an empty form or when some
-- part of the form is unknown.
segmentForm :: Language -> Text -> Maybe [Text]
segmentForm lang form
  | T.null form = Nothing
  | otherwise = fmap (\(_, _, pieces) -> reverse pieces) (Map.lookup (T.length form) table)
  where
    table = foldl extend (Map.singleton 0 (0, 0, [])) [1 .. T.length form]
    extend :: Map Int (Int, Double, [Text]) -> Int -> Map Int (Int, Double, [Text])
    extend best i =
      case [ (count + 1, weight + pieceWeight, piece : pieces)
           | j <- [0 .. i - 1]
           , Just (count, weight, pieces) <- [Map.lookup j best]
           , let piece = T.take (i - j) (T.drop j form)
           , let pieceWeight = formWeight lang piece
           , pieceWeight > 0
           ] of
        [] -> best
        found -> Map.insert i (minimumBy (comparing (\(c, w, _) -> (c, Down w))) found) best

-- | 'SplitMorph' commands that cut every unglossed single-morph token into the
-- pieces given by 'segmentForm'. Tokens that already have several morphs,
-- tokens with a glossed morph and forms that cannot be split into at least
-- two known pieces are left alone. New identifiers start at 'nextFreeId'. The
-- commands are not applied and must be applied in the order returned.
suggestSegmentation :: Language -> Document -> [Command]
suggestSegmentation lang doc = concat (snd (mapAccumL commandsFor (nextFreeId doc) singles))
  where
    singles =
      [ m
      | s <- documentSegments doc
      , t <- segmentTokens s
      , [m] <- [tokenMorphs t]
      , Nothing <- [morphGloss m]
      ]
    commandsFor next m = case segmentForm lang (morphForm m) of
      Just pieces@(_ : _ : _) ->
        (next + fromIntegral (length pieces - 1), splitsFor (morphId m) next pieces)
      _ -> (next, [])

splitsFor :: MorphId -> Word64 -> [Text] -> [Command]
splitsFor mid next pieces =
  zipWith3 SplitMorph (mid : newIds) (map T.length (init pieces)) newIds
  where
    newIds = map MorphId [next ..]

formWeight :: Language -> Text -> Double
formWeight lang form =
  maybe 0 (sum . filter (> 0) . Map.elems) (Map.lookup form (languageWeights lang))
