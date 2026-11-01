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
  , ("conllu writes the UD columns it read", fmap renderConllu (parseConllu udText) == Right udText)
  , ("conllu writes the UD columns of a range", fmap renderConllu (parseConllu rangeText) == Right rangeText)
  , ("conllu writes a head from another sentence as an underscore", renderConllu crossDoc == T.unlines [row "1" "a", "", urow "1" "b" ["_", "_", "_", "_", "_", "dep", "_", "_"], ""])
  , ("conllu writes a tab in a UD column as a space", renderConllu tabDoc == T.unlines [urow "1" "a" ["x y", "_", "_", "_", "_", "_", "_", "_"], ""])
  , ("conllu renumbers heads after an empty node", fmap renderConllu (parseConllu (T.unlines [row "1" "a", row "1.1" "e", urow "2" "b" ["_", "_", "_", "_", "1", "dep", "_", "_"]])) == Right (T.unlines [row "1" "a", urow "2" "b" ["_", "_", "_", "_", "1", "dep", "_", "_"], ""]))
  , ("conllu writes the gloss of a morph in MISC", renderConllu baseDoc == T.unlines [row "1-2" "китаплар", miscRow "1" "китап" "Gloss=book", miscRow "2" "лар" "Gloss=PL", row "3" "мин", ""])
  , ("conllu escapes a gloss", renderConllu (glossedWith (Gloss "a b|c%d" ["PL"])) == T.unlines [miscRow "1" "x" "Gloss=a%20b%7Cc%25d.PL", ""])
  , ("conllu escapes tabs and line breaks in a gloss", renderConllu (glossedWith (Gloss "a\tb\nc\rd" [])) == T.unlines [miscRow "1" "x" "Gloss=a%09b%0Ac%0Dd", ""])
  , ("conllu writes the gloss after the other MISC items", renderConllu (glossedUd "SpaceAfter=No" (Gloss "book" [])) == T.unlines [urow "1" "x" ["_", "_", "_", "_", "_", "_", "_", "SpaceAfter=No|Gloss=book"], ""])
  , ("conllu replaces a Gloss item of MISC with the gloss", renderConllu (glossedUd "Gloss=old|X=1" (Gloss "new" [])) == T.unlines [urow "1" "x" ["_", "_", "_", "_", "_", "_", "_", "X=1|Gloss=new"], ""])
  , ("conllu keeps a Gloss item of MISC for an unglossed morph", fmap renderConllu (parseConllu (T.unlines [miscRow "1" "x" "Gloss=old"])) == Right (T.unlines [miscRow "1" "x" "Gloss=old", ""]))
  , ("parseConlluWith reads the glosses that renderConllu wrote", fmap glosses (parseConlluWith tinyLanguage (renderConllu baseDoc)) == Right [Just (Gloss "book" []), Just (Gloss "" ["PL"]), Nothing])
  , ("parseConlluWith splits a gloss at periods with the tags of the language", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss=book.PL"])) == Right [Just (Gloss "book" ["PL"])])
  , ("parseConlluWith takes a leading tag of the language as a tag", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss=PL.3SG"])) == Right [Just (Gloss "" ["PL", "3SG"])])
  , ("parseConlluWith reads an escaped gloss", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss=a%20b%7Cc%25d%09e%0Af%0Dg"])) == Right [Just (Gloss "a b|c%d\te\nf\rg" [])])
  , ("parseConlluWith keeps text after a percent sign that is not an escape", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss=100%zz%2"])) == Right [Just (Gloss "100%zz%2" [])])
  , ("parseConlluWith leaves the Gloss item out of the UD annotation", uds' (T.unlines [urow "1" "x" ["a", "_", "_", "_", "_", "_", "_", "SpaceAfter=No|Gloss=book|X=1"]]) == Right [Just (UdFields "a" "" "" "" NoHead "" "" "SpaceAfter=No|X=1")])
  , ("parseConlluWith leaves a word with only a gloss without UD annotation", uds' (T.unlines [miscRow "1" "x" "Gloss=book"]) == Right [Nothing])
  , ("parseConlluWith uses the first Gloss item", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss=a|Gloss=b"])) == Right [Just (Gloss "a" [])])
  , ("parseConlluWith leaves a morph with an empty Gloss item unglossed", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss="])) == Right [Nothing])
  , ("parseConlluWith leaves a morph with the gloss ? unglossed", fmap glosses (parseConlluWith tinyLanguage (T.unlines [miscRow "1" "x" "Gloss=?"])) == Right [Nothing])
  , ("parseConlluWith reads the glosses of the words of a range", fmap glosses (parseConlluWith tinyLanguage (T.unlines [row "1-2" "vámonos", miscRow "1" "vamos" "Gloss=go", miscRow "2" "nos" "Gloss=PL"])) == Right [Just (Gloss "go" []), Just (Gloss "" ["PL"])])
  , ("parseConllu does not read a Gloss item as a gloss", fmap glosses (parseConllu (T.unlines [miscRow "1" "x" "Gloss=book"])) == Right [Nothing])
  , ("parseConlluWith keeps a document without Gloss items as parseConllu does", parseConlluWith tinyLanguage udText == parseConllu udText)
  ]
  where
    miscRow i f misc = urow i f (replicate 7 "_" ++ [misc])
    glosses = map morphGloss . concatMap tokenMorphs . concatMap segmentTokens . documentSegments
    uds' t = fmap (map morphUd . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseConlluWith tinyLanguage t)
    oneWord = Document 3 [buildSegment emptyDocument [("x", ["x"])]]
    glossedWith g = fst (must (apply (SetGloss (MorphId 2) (Just g)) oneWord))
    glossedUd misc g = fst (must (apply (SetGloss (MorphId 2) (Just g)) (fst (must (apply (SetUd (MorphId 2) (Just (UdFields "" "" "" "" NoHead "" "" misc))) oneWord)))))
    split = documentOf [("китаплар", ["китап", "лар"]), ("бар", ["бар"])]
    oddForms = documentOf [("a \tb", ["a"]), ("", [])]
    documentOf specs = Document 100 [buildSegment emptyDocument specs]
    sample = T.unlines [T.pack "# text = Мин китап", row "1" "Мин", row "2" "китап"]
    row i f = T.intercalate "\t" [T.pack i, T.pack f, "_", "_", "_", "_", "_", "_", "_", "_"]
    udText = T.unlines [urow "1" "Мин" ["мин", "PRON", "_", "Number=Sing", "2", "nsubj", "_", "SpaceAfter=No"], urow "2" "укыдым" ["укы", "VERB", "_", "_", "0", "root", "_", "_"], ""]
    rangeText = T.unlines [urow "1-2" "vámonos" (replicate 8 "_"), urow "1" "vamos" ["ir", "VERB", "_", "_", "0", "root", "_", "_"], urow "2" "nos" ["nosotros", "PRON", "_", "_", "1", "obj", "_", "_"], ""]
    crossDoc = fst (must (apply (SetUd (MorphId 5) (Just (UdFields "" "" "" "" (HeadOf (MorphId 2)) "dep" "" ""))) (must (parseConllu (T.unlines [row "1" "a", "", row "1" "b"])))))
    tabDoc = fst (must (apply (SetUd (MorphId 2) (Just (UdFields "x\ty" "" "" "" NoHead "" "" ""))) (must (parseConllu (T.unlines [row "1" "a"])))))
    urow i f cols = T.intercalate "\t" ([T.pack i, T.pack f] ++ cols)
    uds t = fmap (map morphUd . concatMap tokenMorphs . concatMap segmentTokens . documentSegments) (parseConllu t)
    morphForms t = fmap (map (map (map morphForm . tokenMorphs) . segmentTokens) . documentSegments) (parseConllu t)
