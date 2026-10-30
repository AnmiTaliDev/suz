-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Language
  ( Language (..)
  , CaseFolding (..)
  , mkLanguage
  , withContext
  , withImportRules
  , candidates
  , contextCandidates
  , candidatesAfter
  , suggest
  , suggestAfter
  , confirm
  , reject
  , confirmAfter
  , rejectAfter
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
  , languageContext :: Map (Text, Text) (Map Gloss Double)
  , languagePunctuation :: Set Char
  , languageCaseFolding :: CaseFolding
  }
  deriving (Eq, Show)

-- | How morph forms are lowercased on import. 'SimpleFolding' is the Unicode
-- lowercase mapping. 'TurkicFolding' maps @I@ to dotless @ı@ and @İ@ to @i@
-- first, as Turkic languages require.
data CaseFolding = NoFolding | SimpleFolding | TurkicFolding
  deriving (Eq, Ord, Show)

-- | Language from a name, the allowed grammatical tags and (form, gloss,
-- weight) entries. Weights of repeated (form, gloss) pairs are summed. The
-- context table is empty.
mkLanguage :: Text -> [Text] -> [(Text, Gloss, Double)] -> Language
mkLanguage name tags entries =
  Language
    { languageName = name
    , languageTags = Set.fromList tags
    , languageWeights =
        Map.fromListWith (Map.unionWith (+)) [(form, Map.singleton g w) | (form, g, w) <- entries]
    , languageContext = Map.empty
    , languagePunctuation = Set.empty
    , languageCaseFolding = NoFolding
    }

-- | Adds (previous form, form, gloss, weight) entries to the context table.
-- Weights of repeated entries are summed with the existing ones.
withContext :: [(Text, Text, Gloss, Double)] -> Language -> Language
withContext entries lang =
  lang
    { languageContext =
        Map.unionWith
          (Map.unionWith (+))
          (languageContext lang)
          (Map.fromListWith (Map.unionWith (+)) [((prev, form), Map.singleton g w) | (prev, form, g, w) <- entries])
    }

-- | Sets the rules used when importing raw text: the characters that are
-- split off the start and end of a word as tokens of their own, and how
-- morph forms are lowercased. Both are off in a language from 'mkLanguage'.
withImportRules :: [Char] -> CaseFolding -> Language -> Language
withImportRules punctuation folding lang =
  lang {languagePunctuation = Set.fromList punctuation, languageCaseFolding = folding}

rank :: Map Gloss Double -> [(Gloss, Double)]
rank = sortBy (comparing (Down . snd) <> comparing fst) . Map.toList

firstPositive :: [(Gloss, Double)] -> Maybe Gloss
firstPositive xs = case filter ((> 0) . snd) xs of
  (g, _) : _ -> Just g
  [] -> Nothing

-- | Glosses known for a morph form, heaviest first. Equal weights are ordered
-- by gloss.
candidates :: Language -> Text -> [(Gloss, Double)]
candidates lang form = rank (Map.findWithDefault Map.empty form (languageWeights lang))

-- | Only the context weights recorded for a form after the given previous
-- form, heaviest first.
contextCandidates :: Language -> Text -> Text -> [(Gloss, Double)]
contextCandidates lang prev form = rank (Map.findWithDefault Map.empty (prev, form) (languageContext lang))

-- | Candidates for a form that follows the given previous form in the same
-- token. The weight of a gloss is its plain weight plus its context weight.
-- With no previous form the result equals 'candidates'.
candidatesAfter :: Language -> Maybe Text -> Text -> [(Gloss, Double)]
candidatesAfter lang Nothing form = candidates lang form
candidatesAfter lang (Just prev) form =
  rank
    ( Map.unionWith
        (+)
        (Map.findWithDefault Map.empty form (languageWeights lang))
        (Map.findWithDefault Map.empty (prev, form) (languageContext lang))
    )

-- | The heaviest candidate for a morph form. Candidates with a weight of zero
-- or less are never suggested.
suggest :: Language -> Text -> Maybe Gloss
suggest lang = firstPositive . candidates lang

-- | 'suggest' with the previous form taken into account, see
-- 'candidatesAfter'.
suggestAfter :: Language -> Maybe Text -> Text -> Maybe Gloss
suggestAfter lang prev = firstPositive . candidatesAfter lang prev

-- | Adds 1 to the weight of a (form, gloss) pair, creating it if needed.
confirm :: Text -> Gloss -> Language -> Language
confirm = adjustWeight 1

-- | Subtracts 1 from the weight of a (form, gloss) pair, creating it if
-- needed. The weight may become negative.
reject :: Text -> Gloss -> Language -> Language
reject = adjustWeight (-1)

-- | Like 'confirm', for the context weight of a gloss after a previous form.
confirmAfter :: Text -> Text -> Gloss -> Language -> Language
confirmAfter = adjustContext 1

-- | Like 'reject', for the context weight of a gloss after a previous form.
rejectAfter :: Text -> Text -> Gloss -> Language -> Language
rejectAfter = adjustContext (-1)

adjustWeight :: Double -> Text -> Gloss -> Language -> Language
adjustWeight delta form g lang =
  lang
    { languageWeights =
        Map.insertWith (Map.unionWith (+)) form (Map.singleton g delta) (languageWeights lang)
    }

adjustContext :: Double -> Text -> Text -> Gloss -> Language -> Language
adjustContext delta prev form g lang =
  lang
    { languageContext =
        Map.insertWith (Map.unionWith (+)) (prev, form) (Map.singleton g delta) (languageContext lang)
    }

-- | True when every tag of the gloss is allowed by the language.
isValidGloss :: Language -> Gloss -> Bool
isValidGloss lang g = all (`Set.member` languageTags lang) (glossTags g)
