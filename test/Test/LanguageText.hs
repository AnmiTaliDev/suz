-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.LanguageText (checks) where

import Fixtures
import Suz

ruled :: Language
ruled = withImportRules "\t\\.«" SimpleFolding (mkLanguage "a" ["PL"] [("x", Gloss "" ["PL"], 1)])

turkicRuled :: Language
turkicRuled = withImportRules "." TurkicFolding (mkLanguage "a" [] [])

checks :: [(String, Bool)]
checks =
  [ ("language text round trip with context", parseLanguage (renderLanguage contextLanguage) == Right contextLanguage)
  , ("language text context line", parseLanguage "language\ta\ncontext\tp\tx\t2\t-\tT\n" == Right (withContext [("p", "x", Gloss "" ["T"], 2)] (mkLanguage "a" [] [])))
  , ("language text rejects short context line", errorOf (parseLanguage "language\ta\ncontext\tp\tx\t1\n") == Just (2, "unrecognized line"))
  , ("language text round trip", parseLanguage (renderLanguage tinyLanguage) == Right tinyLanguage)
  , ("language text import rules", parseLanguage "language\ta\npunctuation\t.,\npunctuation\t!\nfoldcase\n" == Right (withImportRules ".,!" SimpleFolding (mkLanguage "a" [] [])))
  , ("language text Turkic folding", parseLanguage "language\ta\nfoldcase\tturkic\n" == Right (withImportRules "" TurkicFolding (mkLanguage "a" [] [])))
  , ("language text Turkic folding round trip", parseLanguage (renderLanguage turkicRuled) == Right turkicRuled)
  , ("language text rejects unknown folding", parseLanguage "language\ta\nfoldcase\tother\n" == Left (2, "unrecognized line"))
  , ("language text import rules round trip", parseLanguage (renderLanguage ruled) == Right ruled)
  , ("language text omits default import rules", renderLanguage (mkLanguage "a" [] []) == "suz-language\t1\nlanguage\ta\n")
  , ("language text round trip with escapes", parseLanguage (renderLanguage trickyLanguage) == Right trickyLanguage)
  , ("language text is stable", fmap renderLanguage (parseLanguage (renderLanguage trickyLanguage)) == Right (renderLanguage trickyLanguage))
  , ("language text placeholders", parseLanguage "language\ta\nweight\tx\t1\t-\tT\nweight\ty\t1\t\\-\n" == Right (mkLanguage "a" [] [("x", Gloss "" ["T"], 1), ("y", Gloss "-" [], 1)]))
  , ("language text skips comments and blanks", parseLanguage "# c\n\nlanguage\ta\n   \n# d\n" == Right (mkLanguage "a" [] []))
  , ("language text sums repeated entries", fmap (`candidates` "x") (parseLanguage "language\ta\nweight\tx\t1\t-\tT\nweight\tx\t2\t-\tT\n") == Right [(Gloss "" ["T"], 3)])
  , ("language text missing name", errorOf (parseLanguage "tag\tPL\n") == Just (0, "missing language line"))
  , ("language text duplicate name", errorOf (parseLanguage "language\ta\nlanguage\tb\n") == Just (2, "duplicate language line"))
  , ("language text rejects NaN", errorOf (parseLanguage "language\ta\nweight\tx\tNaN\t-\n") == Just (2, "bad weight: NaN"))
  , ("language text rejects bad number", errorOf (parseLanguage "language\ta\nweight\tx\tone\t-\n") == Just (2, "bad weight: one"))
  , ("language text rejects unknown line", errorOf (parseLanguage "language\ta\nbogus\n") == Just (2, "unrecognized line"))
  , ("language text rejects short weight line", errorOf (parseLanguage "language\ta\nweight\tx\t1\n") == Just (2, "unrecognized line"))
  ]
