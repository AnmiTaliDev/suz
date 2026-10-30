-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Import (checks) where

import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("import splits lines and words", importShape == [["Мин", "китап", "укыдым."], ["Син"]])
  , ("import gives one unglossed morph per word", all (\t -> map morphForm (tokenMorphs t) == [tokenSurface t] && all ((== Nothing) . morphGloss) (tokenMorphs t)) importTokens)
  , ("import gives a valid document", null (validateDocument tinyLanguage imported))
  , ("import survives a snapshot", parseSnapshot (renderSnapshot imported) == Right imported)
  , ("import of empty text is empty", importText "  \n\n" == emptyDocument)
  , ("import then edit", errorOf (apply (SplitMorph (morphId (head importMorphs)) 1 (MorphId (nextFreeId imported))) imported) == Nothing)
  ]
  where
    imported = importText "Мин китап укыдым.\n\n  \nСин\n"
    importShape = [map tokenSurface (segmentTokens s) | s <- documentSegments imported]
    importTokens = concatMap segmentTokens (documentSegments imported)
    importMorphs = concatMap tokenMorphs importTokens
