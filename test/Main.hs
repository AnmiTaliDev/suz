-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import qualified Test.Codec as Codec
import qualified Test.Command as Command
import qualified Test.Import as Import
import qualified Test.Journal as Journal
import qualified Test.Language as Language
import qualified Test.LanguageBinary as LanguageBinary
import qualified Test.LanguageText as LanguageText
import qualified Test.Learn as Learn
import qualified Test.Render as Render
import qualified Test.Segment as Segment
import qualified Test.Snapshot as Snapshot
import qualified Test.Suggest as Suggest
import qualified Test.Toolbox as Toolbox
import qualified Test.Validate as Validate
import System.Exit (exitFailure, exitSuccess)

checks :: [(String, Bool)]
checks = concat
  [ Command.checks
  , Codec.checks
  , Journal.checks
  , Language.checks
  , Suggest.checks
  , Render.checks
  , Validate.checks
  , Segment.checks
  , LanguageText.checks
  , LanguageBinary.checks
  , Snapshot.checks
  , Learn.checks
  , Import.checks
  , Toolbox.checks
  ]

main :: IO ()
main = do
  let failed = [name | (name, False) <- checks]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  putStrLn (show (length checks - length failed) ++ "/" ++ show (length checks) ++ " passed")
  if null failed then exitSuccess else exitFailure
