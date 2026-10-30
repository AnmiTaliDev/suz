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
  , ("import with rules splits punctuation", surfaces "«Мин, китап!»" == ["«", "Мин", ",", "китап", "!", "»"])
  , ("import with rules keeps inner punctuation", surfaces "а-б, в.г" == ["а-б", ",", "в.г"])
  , ("import with rules handles punctuation-only words", surfaces "... Мин" == [".", ".", ".", "Мин"])
  , ("import with rules lowercases morph forms only", morphsOf (withImportRules "" SimpleFolding plain) "Мин" == [("Мин", "мин")])
  , ("import with simple folding maps I to i", morphsOf (withImportRules "" SimpleFolding plain) "I\x130" == [("I\x130", "i\x69\x307")])
  , ("import with Turkic folding maps I to dotless i", morphsOf turkic "ISIK" == [("ISIK", "\x131s\x131k")])
  , ("import with Turkic folding maps dotted I to i", morphsOf turkic "\x130stanbul" == [("\x130stanbul", "istanbul")])
  , ("import with Turkic folding lowercases other letters", morphsOf turkic "Мин" == [("Мин", "мин")])
  , ("import without rules keeps case", morphsOf plain "Мин" == [("Мин", "Мин")])
  , ("import with empty rules equals importText", importTextWith plain "Мин китап укыдым.\n\n  \nСин\n" == imported)
  , ("import with rules gives a valid document", null (validateDocument tinyLanguage (importTextWith punctuating "Мин, китап!")))
  , ("import then edit", errorOf (apply (SplitMorph (morphId (importMorphs !! 0)) 1 (MorphId (nextFreeId imported))) imported) == Nothing)
  ]
  where
    imported = importText "Мин китап укыдым.\n\n  \nСин\n"
    importShape = [map tokenSurface (segmentTokens s) | s <- documentSegments imported]
    importTokens = concatMap segmentTokens (documentSegments imported)
    importMorphs = concatMap tokenMorphs importTokens
    plain = mkLanguage "p" [] []
    turkic = withImportRules "" TurkicFolding plain
    punctuating = withImportRules ".,!«»" NoFolding plain
    surfaces t = map tokenSurface (concatMap segmentTokens (documentSegments (importTextWith punctuating t)))
    morphsOf lang t = [(tokenSurface tk, morphForm m) | tk <- concatMap segmentTokens (documentSegments (importTextWith lang t)), m <- tokenMorphs tk]
