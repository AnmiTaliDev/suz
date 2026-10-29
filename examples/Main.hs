-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import Control.Monad (foldM)
import qualified Data.Text.IO as T
import English (english)
import Suz
import System.Exit (die)
import System.IO (hSetEncoding, stdout, utf8)
import Tatar (tatar)

must :: Show e => Either e a -> IO a
must = either (die . show) pure

main :: IO ()
main = do
  hSetEncoding stdout utf8
  tatarExample
  putStrLn ""
  englishExample

tatarExample :: IO ()
tatarExample = do
  putStrLn "== Tatar: suggestions, undo, journal"
  let segment =
        buildSegment
          emptyDocument
          [("Мин", ["мин"]), ("китапларны", ["китап", "лар", "ны"]), ("укыдым", ["укы", "ды", "м"])]
  inserted <- must (perform (InsertSegment 0 segment) (newSession emptyDocument))
  glossed <- must (foldM (flip perform) inserted (suggestGlosses tatar (sessionDocument inserted)))
  T.putStrLn (renderDocument (sessionDocument glossed))
  putStrLn "-- after one undo"
  reverted <- must (undo glossed)
  T.putStrLn (renderDocument (sessionDocument reverted))
  putStrLn "-- journal"
  T.putStr (renderJournal glossed)

englishExample :: IO ()
englishExample = do
  putStrLn "== English: correcting a suggestion and confirming it"
  let segment = buildSegment emptyDocument [("She", ["she"]), ("walks", ["walk", "s"])]
  inserted <- must (perform (InsertSegment 0 segment) (newSession emptyDocument))
  glossed <- must (foldM (flip perform) inserted (suggestGlosses english (sessionDocument inserted)))
  T.putStrLn (renderDocument (sessionDocument glossed))
  let suffix = morphId (last (tokenMorphs (last (segmentTokens segment))))
      correct = Gloss "" ["3SG"]
  fixed <- must (perform (SetGloss suffix (Just correct)) glossed)
  putStrLn "-- after correction"
  T.putStrLn (renderDocument (sessionDocument fixed))
  putStrLn "-- suggestion for \"s\" before and after confirm"
  print (fmap renderGloss (suggest english "s"))
  print (fmap renderGloss (suggest (confirm "s" correct english) "s"))
