-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Language
  ( Language (..)
  , mkLanguage
  , candidates
  , suggest
  , confirm
  , isValidGloss
  ) where

import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (Down (..), comparing)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import Suz.Model

data Language = Language
  { languageName :: Text
  , languageTags :: Set Text
  , languageWeights :: Map Text (Map Gloss Double)
  }
  deriving (Eq, Show)

-- | Language from a name, the allowed grammatical tags and (form, gloss,
-- weight) entries. Weights of repeated (form, gloss) pairs are summed.
mkLanguage :: Text -> [Text] -> [(Text, Gloss, Double)] -> Language
mkLanguage name tags entries =
  Language
    { languageName = name
    , languageTags = Set.fromList tags
    , languageWeights =
        Map.fromListWith (Map.unionWith (+)) [(form, Map.singleton g w) | (form, g, w) <- entries]
    }

-- | Glosses known for a morph form, heaviest first. Equal weights are ordered
-- by gloss.
candidates :: Language -> Text -> [(Gloss, Double)]
candidates lang form =
  sortBy (comparing (Down . snd) <> comparing fst) (maybe [] Map.toList (Map.lookup form (languageWeights lang)))

-- | The heaviest candidate for a morph form.
suggest :: Language -> Text -> Maybe Gloss
suggest lang form = case candidates lang form of
  (g, _) : _ -> Just g
  [] -> Nothing

-- | Adds 1 to the weight of a (form, gloss) pair, creating it if needed.
confirm :: Text -> Gloss -> Language -> Language
confirm form g lang =
  lang
    { languageWeights =
        Map.insertWith (Map.unionWith (+)) form (Map.singleton g 1) (languageWeights lang)
    }

-- | True when every tag of the gloss is allowed by the language.
isValidGloss :: Language -> Gloss -> Bool
isValidGloss lang g = all (`Set.member` languageTags lang) (glossTags g)
