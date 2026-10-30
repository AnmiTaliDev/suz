-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Conllu (checks) where

import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("conllu sentence", forms sample == Right [[["Мин"], ["китап"]]])
  , ("conllu two sentences", fmap (length . documentSegments) (parseConllu (sample <> "\n" <> sample)) == Right 2)
  , ("conllu morphs are unglossed", fmap (all ((== Nothing) . morphGloss) . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseConllu sample) == Right True)
  , ("conllu multiword token", forms (T.unlines [row "1-2" "vámonos", row "1" "vamos", row "2" "nos", row "3" "ya"]) == Right [[["vamos", "nos"], ["ya"]]])
  , ("conllu multiword surface", fmap (map tokenSurface . concatMap segmentTokens . documentSegments) (parseConllu (T.unlines [row "1-2" "vámonos", row "1" "vamos", row "2" "nos"])) == Right ["vámonos"])
  , ("conllu skips empty nodes", forms (T.unlines [row "1" "a", row "1.1" "e", row "2" "b"]) == Right [[["a"], ["b"]]])
  , ("conllu skips sentences without words", fmap (length . documentSegments) (parseConllu (T.unlines [row "1.1" "e"])) == Right 0)
  , ("conllu ids are consecutive", fmap documentIds (parseConllu sample) == Right (documentIds (importText "Мин китап")))
  , ("conllu rejects a short line", errorOf (parseConllu "1\ta\n") == Just (1, "expected 10 columns"))
  , ("conllu rejects a bad id", errorOf (parseConllu (row "x" "a")) == Just (1, "bad ID"))
  ]
  where
    sample = T.unlines [T.pack "# text = Мин китап", row "1" "Мин", row "2" "китап"]
    row i f = T.intercalate "\t" [T.pack i, T.pack f, "_", "_", "_", "_", "_", "_", "_", "_"]
    forms t = fmap (map (map (map morphForm . tokenMorphs) . segmentTokens) . documentSegments) (parseConllu t)
