-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import Azerbaijani (azerbaijani)
import Control.Monad (foldM)
import CrimeanTatar (crimeanTatar)
import Data.Text (Text)
import qualified Data.Text.IO as T
import English (english)
import Kazakh (kazakh)
import Kyrgyz (kyrgyz)
import Russian (russian)
import Suz
import System.Exit (die)
import System.IO (hSetEncoding, stdout, utf8)
import Tatar (tatar)
import Turkish (turkish)
import Uzbek (uzbek)

must :: Show e => Either e a -> IO a
must = either (die . show) pure

main :: IO ()
main = do
  hSetEncoding stdout utf8
  tatarExample
  putStrLn ""
  englishExample
  putStrLn ""
  rawTextExample "Turkish" turkish "Işık kitapları okudum."
  putStrLn ""
  rawTextExample "Kazakh" kazakh "Мен кітаптарды оқыдым."
  putStrLn ""
  rawTextExample "Uzbek" uzbek "Men kitoblarni o'qidim."
  putStrLn ""
  rawTextExample "Kyrgyz" kyrgyz "Мен китептерди окудум."
  putStrLn ""
  rawTextExample "Azerbaijani" azerbaijani "İşıq kitabları oxudum."
  putStrLn ""
  rawTextExample "Crimean Tatar" crimeanTatar "Men kitaplarnı oqudım."
  putStrLn ""
  rawTextExample "Russian" russian "Я читаю книги."

tatarExample :: IO ()
tatarExample = do
  putStrLn "== Tatar: segmentation, suggestions, undo, journal"
  let segment =
        buildSegment
          emptyDocument
          [("Мин", ["мин"]), ("китапларны", ["китапларны"]), ("укыдым", ["укыдым"])]
  inserted <- must (perform (InsertSegment 0 segment) (newSession emptyDocument))
  segmented <- must (foldM (flip perform) inserted (suggestSegmentation tatar (sessionDocument inserted)))
  glossed <- must (foldM (flip perform) segmented (suggestGlosses tatar (sessionDocument segmented)))
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
  (fixed, learnedOnce) <- must (performLearning (SetGloss suffix (Just correct)) english glossed)
  putStrLn "-- after correction"
  T.putStrLn (renderDocument (sessionDocument fixed))
  let cats = buildSegment (sessionDocument fixed) [("cats", ["cat", "s"])]
  [catMorph, pluralMorph] <- pure (concatMap tokenMorphs (segmentTokens cats))
  withCats <- must (perform (InsertSegment 1 cats) fixed)
  (_, learned) <-
    must
      ( performLearning (SetGloss (morphId catMorph) (Just (Gloss "cat" []))) learnedOnce withCats
          >>= \(session, lang) -> performLearning (SetGloss (morphId pluralMorph) (Just (Gloss "" ["PL"]))) lang session
      )
  putStrLn "-- suggestion for \"s\": before learning, with no context, after \"walk\", after \"cat\""
  print (fmap renderGloss (suggest english "s"))
  print (fmap renderGloss (suggest learned "s"))
  print (fmap renderGloss (suggestAfter learned (Just "walk") "s"))
  print (fmap renderGloss (suggestAfter learned (Just "cat") "s"))

rawTextExample :: String -> Language -> Text -> IO ()
rawTextExample name lang text = do
  putStrLn ("== " ++ name ++ ": raw text import, segmentation and suggestions")
  let imported = newSession (importTextWith lang text)
  segmented <- must (foldM (flip perform) imported (suggestSegmentation lang (sessionDocument imported)))
  glossed <- must (foldM (flip perform) segmented (suggestGlosses lang (sessionDocument segmented)))
  T.putStrLn (renderDocument (sessionDocument glossed))
