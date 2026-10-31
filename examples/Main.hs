-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as T
import Paths_suz (getDataFileName)
import Suz
import System.Exit (die)
import System.IO (hSetEncoding, stdout, utf8)

must :: Show e => Either e a -> IO a
must = either (die . show) pure

loadLanguage :: String -> IO Language
loadLanguage name = do
  path <- getDataFileName ("languages/" ++ name ++ ".language")
  bytes <- BS.readFile path
  either (die . show) pure (parseLanguage (TE.decodeUtf8 bytes))

main :: IO ()
main = do
  hSetEncoding stdout utf8
  tatar <- loadLanguage "tatar"
  english <- loadLanguage "english"
  tatarExample tatar
  putStrLn ""
  englishExample english
  mapM_
    ( \(file, name, text) -> do
        lang <- loadLanguage file
        putStrLn ""
        rawTextExample name lang text
    )
    [ ("turkish", "Turkish", "Işık kitapları okudum.")
    , ("kazakh", "Kazakh", "Мен кітаптарды оқыдым.")
    , ("uzbek", "Uzbek", "Men kitoblarni o'qidim.")
    , ("kyrgyz", "Kyrgyz", "Мен китептерди окудум.")
    , ("azerbaijani", "Azerbaijani", "İşıq kitabları oxudum.")
    , ("crimean-tatar", "Crimean Tatar", "Men kitaplarnı oqudım.")
    , ("russian", "Russian", "Я читаю книги.")
    , ("turkmen", "Turkmen", "Men kitaplary okadym.")
    ]

tatarExample :: Language -> IO ()
tatarExample tatar = do
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

englishExample :: Language -> IO ()
englishExample english = do
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
