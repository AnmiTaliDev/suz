-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import Control.Monad (foldM)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz
import System.Exit (exitFailure, exitSuccess)

must :: Show e => Either e a -> a
must = either (error . show) id

errorOf :: Either e a -> Maybe e
errorOf = either Just (const Nothing)

rawToken :: Token -> Word64
rawToken t = case tokenId t of TokenId n -> n

plainSegment :: Segment
plainSegment = buildSegment emptyDocument [("китаплар", ["китап", "лар"]), ("мин", ["мин"])]

plainDoc :: Document
plainDoc = fst (must (apply (InsertSegment 0 plainSegment) emptyDocument))

tokA, tokB :: Token
tokA = segmentTokens plainSegment !! 0
tokB = segmentTokens plainSegment !! 1

mA1, mA2 :: MorphId
mA1 = morphId (tokenMorphs tokA !! 0)
mA2 = morphId (tokenMorphs tokA !! 1)

glossBook, glossPl :: Gloss
glossBook = Gloss "book" []
glossPl = Gloss "" ["PL"]

baseDoc :: Document
baseDoc =
  fst (must (apply (Batch [SetGloss mA1 (Just glossBook), SetGloss mA2 (Just glossPl)]) plainDoc))

spareMorph :: Morph
spareMorph = Morph (MorphId (nextFreeId baseDoc)) "х" Nothing

commands :: [Command]
commands =
  [ InsertSegment 1 (buildSegment baseDoc [("a", ["a", "b"])])
  , RemoveSegment (segmentId plainSegment)
  , InsertToken (segmentId plainSegment) 1 (buildToken baseDoc "ва" ["ва"])
  , RemoveToken (tokenId tokA)
  , SetSurface (tokenId tokA) "Китаплар"
  , InsertMorph (tokenId tokA) 2 spareMorph
  , RemoveMorph mA1
  , SetForm mA1 "кит"
  , SetGloss mA1 (Just (Gloss "x" ["A", "B"]))
  , SetGloss mA1 Nothing
  , SplitMorph mA1 2 (morphId spareMorph)
  , MergeMorph mA1
  , Batch [SetForm mA1 "x", SetGloss mA1 Nothing, RemoveMorph mA2]
  , SetSurface (tokenId tokB) "a\tb\nc\\d\r"
  ]

roundTrips :: Command -> Bool
roundTrips c = case apply c baseDoc >>= \(d, inverse) -> apply inverse d of
  Right (d', _) -> documentSegments d' == documentSegments baseDoc
  Left _ -> False

forms :: Document -> [String]
forms doc =
  [ T.unpack (morphForm m)
  | s <- documentSegments doc
  , t <- segmentTokens s
  , m <- tokenMorphs t
  ]

after :: Command -> Document
after c = fst (must (apply c baseDoc))

session2 :: Session
session2 =
  must $
    foldM
      (flip perform)
      (newSession emptyDocument)
      [InsertSegment 0 plainSegment, SetForm mA1 "ки\tт\\ап\nx"]

tinyLanguage :: Language
tinyLanguage =
  mkLanguage
    "tiny"
    ["PL", "3SG"]
    [ ("s", glossPl, 5)
    , ("s", Gloss "" ["3SG"], 4)
    , ("s", glossPl, 0.5)
    , ("китап", glossBook, 3)
    , ("лар", glossPl, 3)
    ]

checks :: [(String, Bool)]
checks =
  [ ("every command is undone by its inverse", all roundTrips commands)
  , ("split forms", forms (after (SplitMorph mA1 2 spareId)) == ["ки", "тап", "лар", "мин"])
  , ("merge forms", forms (after (MergeMorph mA1)) == ["китаплар", "мин"])
  , ("merge keeps first gloss", firstGloss (after (MergeMorph mA1)) == Just glossBook)
  , ("remove then insert restores position", roundTrips (RemoveToken (tokenId tokA)))
  , ("unknown token", errorOf (apply (RemoveToken (TokenId 999)) baseDoc) == Just (NoSuchToken (TokenId 999)))
  , ( "duplicate id"
    , errorOf (apply (InsertToken (segmentId plainSegment) 0 tokA) baseDoc)
        == Just (DuplicateId (rawToken tokA))
    )
  , ( "index out of range"
    , errorOf (apply (InsertToken (segmentId plainSegment) 5 (buildToken baseDoc "q" [])) baseDoc)
        == Just (IndexOutOfRange 5)
    )
  , ("split at zero", errorOf (apply (SplitMorph mA1 0 spareId) baseDoc) == Just (BadSplitPoint mA1 0))
  , ("split at end", errorOf (apply (SplitMorph mA1 5 spareId) baseDoc) == Just (BadSplitPoint mA1 5))
  , ("merge last morph", errorOf (apply (MergeMorph mA2) baseDoc) == Just (NoNextMorph mA2))
  , ( "failed batch changes nothing"
    , errorOf (apply (Batch [SetForm mA1 "z", RemoveMorph (MorphId 999)]) baseDoc) == Just (NoSuchMorph (MorphId 999))
    )
  , ("ids are not reused after removal", nextFreeId (after (RemoveSegment (segmentId plainSegment))) == nextFreeId baseDoc)
  , ("codec round trip", all (\c -> decodeCommand (encodeCommand c) == Right c) commands)
  , ("codec rejects unknown command", errorOf (decodeCommand "bogus") /= Nothing)
  , ("codec rejects trailing fields", errorOf (decodeCommand "remove-token\t1\t2") /= Nothing)
  , ("codec rejects bad escape", errorOf (decodeCommand "set-form\t1\ta\\q") /= Nothing)
  , ("undo twice reaches the empty document", fmap (documentSegments . sessionDocument) (undo session2 >>= undo) == Right [])
  , ("undo on a fresh session", errorOf (undo (newSession emptyDocument)) == Just NothingToUndo)
  , ("redo restores the document", fmap sessionDocument (undo session2 >>= redo) == Right (sessionDocument session2))
  , ("perform clears redo", errorOf (undo session2 >>= performAny (SetForm mA1 "q") >>= redo) == Just NothingToRedo)
  , ("journal replay", fmap sessionDocument replayed == Right (sessionDocument session2))
  , ("journal replay keeps history", fmap history replayed == Right (history session2))
  , ("replay parse error", replayParseLine1)
  , ("replay edit error", replayJournal emptyDocument "\nremove-token\t9\n" == Left (ReplayEdit 2 (NoSuchToken (TokenId 9))))
  , ("candidates order", candidates tinyLanguage "s" == [(glossPl, 5.5), (Gloss "" ["3SG"], 4)])
  , ("unknown form", null (candidates tinyLanguage "zzz") && suggest tinyLanguage "zzz" == Nothing)
  , ("confirm changes ranking", suggest (confirmTwice tinyLanguage) "s" == Just (Gloss "" ["3SG"]))
  , ("valid gloss", isValidGloss tinyLanguage glossPl && not (isValidGloss tinyLanguage (Gloss "" ["ACC"])))
  , ("suggest fills unglossed morphs", suggestions == 2)
  , ("suggest skips glossed morphs", null (suggestGlosses tinyLanguage baseDoc))
  , ("suggested glosses apply", glossesAfterSuggest == [Just glossBook, Just glossPl, Nothing])
  , ("render", renderDocument baseDoc == "китаплар  мин\nкитап-лар мин\nbook-PL   ?")
  ]
  where
    spareId = MorphId (nextFreeId baseDoc)
    firstGloss doc = morphGloss (head (concatMap tokenMorphs (concatMap segmentTokens (documentSegments doc))))
    performAny c = either (Left . JournalEdit) Right . perform c
    replayed = replayJournal emptyDocument (renderJournal session2)
    replayParseLine1 = case replayJournal emptyDocument "bogus\n" of
      Left (ReplayParse 1 _) -> True
      _ -> False
    confirmTwice = confirm "s" (Gloss "" ["3SG"]) . confirm "s" (Gloss "" ["3SG"])
    suggestions = length (suggestGlosses tinyLanguage plainDoc)
    glossesAfterSuggest =
      let doc = fst (must (apply (Batch (suggestGlosses tinyLanguage plainDoc)) plainDoc))
       in [morphGloss m | s <- documentSegments doc, t <- segmentTokens s, m <- tokenMorphs t]

main :: IO ()
main = do
  let failed = [name | (name, False) <- checks]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  putStrLn (show (length checks - length failed) ++ "/" ++ show (length checks) ++ " passed")
  if null failed then exitSuccess else exitFailure
