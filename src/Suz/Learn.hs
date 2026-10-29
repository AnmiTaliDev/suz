-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Learn
  ( learnChange
  , performLearning
  ) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Suz.Command
import Suz.Journal
import Suz.Language
import Suz.Model

type Placed = (Maybe Text, Morph)

placedMorphs :: Document -> [Placed]
placedMorphs doc =
  [ placed
  | s <- documentSegments doc
  , t <- segmentTokens s
  , placed <- zip (Nothing : map (Just . morphForm) (tokenMorphs t)) (tokenMorphs t)
  ]

-- | Updates a language from the difference between two states of a document.
-- A morph whose gloss was set or replaced confirms the new (form, gloss)
-- pair, and the pair after the previous morph of the same token when there is
-- one. A morph whose gloss was replaced or cleared rejects the old pair the
-- same way, using the form and the previous form it had before. Morphs with an
-- unchanged gloss, morphs that were removed and changes that do not touch
-- glosses leave the language as it is.
learnChange :: Document -> Document -> Language -> Language
learnChange before after lang = foldl step lang (placedMorphs after)
  where
    earlier :: Map MorphId Placed
    earlier = Map.fromList [(morphId m, p) | p@(_, m) <- placedMorphs before]
    step l (prev, m)
      | oldGloss == morphGloss m = l
      | otherwise = confirmNew (rejectOld l)
      where
        old = Map.lookup (morphId m) earlier
        oldGloss = old >>= morphGloss . snd
        rejectOld = case (old, oldGloss) of
          (Just (oldPrev, oldMorph), Just g) -> adjust rejectAfter reject oldPrev (morphForm oldMorph) g
          _ -> id
        confirmNew = case morphGloss m of
          Just g -> adjust confirmAfter confirm prev (morphForm m) g
          Nothing -> id
    adjust withPrev plain prev form g =
      maybe id (\p -> withPrev p form g) prev . plain form g

-- | Applies a command like 'perform' and, when it succeeds, updates the
-- language with 'learnChange'. Call it for edits made by a person. Undoing
-- the command later does not revert the language.
performLearning :: Command -> Language -> Session -> Either EditError (Session, Language)
performLearning command lang session = do
  session' <- perform command session
  pure (session', learnChange (sessionDocument session) (sessionDocument session') lang)
