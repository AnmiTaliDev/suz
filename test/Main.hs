-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import qualified Test.Codec as Codec
import qualified Test.Command as Command
import qualified Test.Conllu as Conllu
import qualified Test.Import as Import
import qualified Test.Journal as Journal
import qualified Test.Language as Language
import qualified Test.LanguageBinary as LanguageBinary
import qualified Test.LanguageText as LanguageText
import qualified Test.Learn as Learn
import qualified Test.Leipzig as Leipzig
import qualified Test.Property as Property
import qualified Test.Render as Render
import qualified Test.Segment as Segment
import qualified Test.Snapshot as Snapshot
import qualified Test.Suggest as Suggest
import qualified Test.Toolbox as Toolbox
import qualified Test.Validate as Validate
import qualified Test.Escape as Escape
import qualified Test.Parser as Parser
import qualified Test.Model as Model
import qualified Test.Edit as Edit
import System.Exit (exitFailure, exitSuccess)
import Test.QuickCheck (Args (..), isSuccess, output, quickCheckWithResult, stdArgs)

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
  , Leipzig.checks
  , Conllu.checks
  , Escape.checks
  , Parser.checks
  , Model.checks
  , Edit.checks
  ]

main :: IO ()
main = do
  let failed = [name | (name, False) <- checks]
  results <- mapM (\(name, p) -> (,) name <$> quickCheckWithResult stdArgs {chatty = False} p) Property.properties
  let failedProperties = [(name, r) | (name, r) <- results, not (isSuccess r)]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  mapM_ (\(name, r) -> putStrLn ("FAIL: property " ++ name) >> putStr (output r)) failedProperties
  putStrLn (show (length checks - length failed) ++ "/" ++ show (length checks) ++ " checks passed")
  putStrLn (show (length results - length failedProperties) ++ "/" ++ show (length results) ++ " properties passed")
  if null failed && null failedProperties then exitSuccess else exitFailure
