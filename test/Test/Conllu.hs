-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Conllu (checks) where

import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("conllu sentence", morphForms sample == Right [[["Мин"], ["китап"]]])
  , ("conllu two sentences", fmap (length . documentSegments) (parseConllu (sample <> "\n" <> sample)) == Right 2)
  , ("conllu morphs are unglossed", fmap (all ((== Nothing) . morphGloss) . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseConllu sample) == Right True)
  , ("conllu multiword token", morphForms (T.unlines [row "1-2" "vámonos", row "1" "vamos", row "2" "nos", row "3" "ya"]) == Right [[["vamos", "nos"], ["ya"]]])
  , ("conllu multiword surface", fmap (map tokenSurface . concatMap segmentTokens . documentSegments) (parseConllu (T.unlines [row "1-2" "vámonos", row "1" "vamos", row "2" "nos"])) == Right ["vámonos"])
  , ("conllu skips empty nodes", morphForms (T.unlines [row "1" "a", row "1.1" "e", row "2" "b"]) == Right [[["a"], ["b"]]])
  , ("conllu skips sentences without words", fmap (length . documentSegments) (parseConllu (T.unlines [row "1.1" "e"])) == Right 0)
  , ("conllu ids are consecutive", fmap documentIds (parseConllu sample) == Right (documentIds (importText "Мин китап")))
  , ("conllu render of imported text", renderConllu (importText "Мин китап\nСин") == T.unlines [row "1" "Мин", row "2" "китап", "", row "1" "Син", ""])
  , ("conllu render of a split token", renderConllu split == T.unlines [row "1-2" "китаплар", row "1" "китап", row "2" "лар", row "3" "бар", ""])
  , ("conllu render escapes whitespace and empty forms", renderConllu (importText "a") == T.unlines [row "1" "a", ""] && renderConllu oddForms == T.unlines [row "1" "a\160\160b", row "2" "_", ""])
  , ("conllu render skips empty segments", renderConllu (Document 1 [Segment (SegmentId 0) []]) == "")
  , ("conllu render is read back", fmap renderConllu (parseConllu (renderConllu split)) == Right (renderConllu split))
  , ("conllu rejects a short line", errorOf (parseConllu "1\ta\n") == Just (1, "expected 10 columns"))
  , ("conllu rejects a bad id", errorOf (parseConllu (row "x" "a")) == Just (1, "bad ID"))
  , ("conllu reads UD columns", uds (T.unlines [urow "1" "Мин" ["мин", "PRON", "_", "Number=Sing", "2", "nsubj", "_", "SpaceAfter=No"], urow "2" "укыдым" ["укы", "VERB", "_", "_", "0", "root", "_", "_"]]) == Right [Just (UdFields "мин" "PRON" "" "Number=Sing" (HeadOf (MorphId 4)) "nsubj" "" "SpaceAfter=No"), Just (UdFields "укы" "VERB" "" "" RootHead "root" "" "")])
  , ("conllu leaves a word with empty UD columns plain", uds (T.unlines [row "1" "a"]) == Right [Nothing])
  , ("conllu reads UD columns of the words of a range", uds (T.unlines [urow "1-2" "vámonos" (replicate 8 "X"), urow "1" "vamos" ["ir", "VERB", "_", "_", "0", "root", "_", "_"], urow "2" "nos" ["nosotros", "PRON", "_", "_", "1", "obj", "_", "_"]]) == Right [Just (UdFields "ir" "VERB" "" "" RootHead "root" "" ""), Just (UdFields "nosotros" "PRON" "" "" (HeadOf (MorphId 2)) "obj" "" "")])
  , ("conllu head counts words only", uds (T.unlines [row "1" "a", row "1.1" "e", urow "2" "b" ["_", "_", "_", "_", "1", "dep", "_", "_"]]) == Right [Nothing, Just (UdFields "" "" "" "" (HeadOf (MorphId 2)) "dep" "" "")])
  , ("conllu heads are local to a sentence", uds (T.unlines [row "1" "a", "", urow "1" "b" ["_", "_", "_", "_", "1", "dep", "_", "_"]]) == Right [Nothing, Just (UdFields "" "" "" "" (HeadOf (MorphId 5)) "dep" "" "")])
  , ("conllu rejects a head outside the sentence", errorOf (parseConllu (T.unlines [urow "1" "a" ["_", "_", "_", "_", "5", "dep", "_", "_"]])) == Just (1, "bad HEAD"))
  , ("conllu rejects a head from another sentence", errorOf (parseConllu (T.unlines [row "1" "a", row "2" "b", "", urow "1" "c" ["_", "_", "_", "_", "2", "dep", "_", "_"]])) == Just (4, "bad HEAD"))
  , ("conllu rejects a head that is not a number", errorOf (parseConllu (T.unlines [urow "1" "a" ["_", "_", "_", "_", "x", "dep", "_", "_"]])) == Just (1, "bad HEAD"))
  ]
  where
    split = documentOf [("китаплар", ["китап", "лар"]), ("бар", ["бар"])]
    oddForms = documentOf [("a \tb", ["a"]), ("", [])]
    documentOf specs = Document 100 [buildSegment emptyDocument specs]
    sample = T.unlines [T.pack "# text = Мин китап", row "1" "Мин", row "2" "китап"]
    row i f = T.intercalate "\t" [T.pack i, T.pack f, "_", "_", "_", "_", "_", "_", "_", "_"]
    urow i f cols = T.intercalate "\t" ([T.pack i, T.pack f] ++ cols)
    uds t = fmap (map morphUd . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseConllu t)
    morphForms t = fmap (map (map (map morphForm . tokenMorphs) . segmentTokens) . documentSegments) (parseConllu t)
