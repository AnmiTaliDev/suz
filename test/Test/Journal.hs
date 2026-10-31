-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Journal (checks) where

import qualified Data.Text as T
import Fixtures
import Suz

checks :: [(String, Bool)]
checks =
  [ ("undo twice reaches the empty document", fmap (documentSegments . sessionDocument) (undo session2 >>= undo) == Right [])
  , ("undo on a fresh session", errorOf (undo (newSession emptyDocument)) == Just NothingToUndo)
  , ("redo restores the document", fmap sessionDocument (undo session2 >>= redo) == Right (sessionDocument session2))
  , ("perform clears redo", errorOf (undo session2 >>= performAny (SetForm mA1 "q") >>= redo) == Just NothingToRedo)
  , ("journal replay", fmap sessionDocument replayed == Right (sessionDocument session2))
  , ("journal replay keeps history", fmap history replayed == Right (history session2))
  , ("replay parse error", replayParseLine1)
  , ("replay edit error", replayJournal emptyDocument "\nremove-token\t9\n" == Left (ReplayEdit 2 (NoSuchToken (TokenId 9))))
  , ("journal version is 1", journalVersion == 1)
  , ("journal starts with its version", "suz-journal\t1\n" `T.isPrefixOf` renderJournal session2)
  , ("journal of a fresh session is its version line", renderJournal (newSession emptyDocument) == "suz-journal\t1\n")
  , ("replay of a journal with only a version line", fmap sessionDocument (replayJournal emptyDocument "suz-journal\t1\n") == Right emptyDocument)
  , ("replay of a journal without a version line", fmap sessionDocument (replayJournal emptyDocument (T.unlines (map encodeCommand (history session2)))) == Right (sessionDocument session2))
  , ("replay rejects another version", replayJournal emptyDocument "suz-journal\t2\n" == Left (ReplayParse 1 "unsupported journal version: 2"))
  , ("replay reports the line of an unsupported version", replayJournal emptyDocument "\nsuz-journal\t2\n" == Left (ReplayParse 2 "unsupported journal version: 2"))
  , ("replay rejects a version that is not a number", replayJournal emptyDocument "suz-journal\tx\n" == Left (ReplayParse 1 "bad version: x"))
  , ("replay rejects a repeated version line", replayJournal emptyDocument "suz-journal\t1\nsuz-journal\t1\n" == Left (ReplayParse 2 "unknown command: suz-journal"))
  , ("perform rejects a head that does not exist", headError mA1 (HeadOf (MorphId 999)) (newSession baseDoc) == Just (BadUdHead (UdDanglingHead mA1 (MorphId 999))))
  , ("perform rejects a head in another segment", headError mA1 (HeadOf mZ) (newSession twoSegmentDoc) == Just (BadUdHead (UdHeadOutsideSegment mA1 mZ)))
  , ("perform rejects a morph that is its own head", headError mA1 (HeadOf mA1) (newSession baseDoc) == Just (BadUdHead (UdHeadCycle mA1)))
  , ("perform rejects a head that closes a cycle", fmap (headError mA2 (HeadOf mA1)) (setHead mA1 (HeadOf mA2) (newSession baseDoc)) == Right (Just (BadUdHead (UdHeadCycle mA1))))
  , ("perform accepts a head in the same segment", isRight (setHead mA1 (HeadOf mB) (newSession baseDoc)))
  , ("perform accepts a root head and no head", isRight (setHead mA1 RootHead (newSession baseDoc)) && isRight (setHead mA1 NoHead (newSession baseDoc)))
  , ("perform rejects an inserted morph with a bad head", errorOf (perform (InsertMorph (tokenId tokA) 2 udMorph {morphUd = Just udSample {udHead = HeadOf (MorphId 999)}}) (newSession baseDoc)) == Just (BadUdHead (UdDanglingHead (morphId udMorph) (MorphId 999))))
  , ("perform accepts an inserted morph with a good head", isRight (perform (InsertMorph (tokenId tokA) 2 udMorph) (newSession baseDoc)))
  , ("perform rejects a move that leaves a head behind", fmap (errorOf . perform (MoveToken (tokenId tokB) (segmentId otherSegment) 0)) (setHead mA1 (HeadOf mB) (newSession twoSegmentDoc)) == Right (Just (BadUdHead (UdHeadOutsideSegment mA1 mB))))
  , ("perform leaves an old invalid head alone", isRight (perform (SetForm mA2 "q") (newSession brokenDoc)))
  , ("perform accepts the fix of an old invalid head", isRight (setHead mA1 RootHead (newSession brokenDoc)))
  , ("undo restores an old invalid head", fmap sessionDocument (either (Left . JournalEdit) undo (setHead mA1 RootHead (newSession brokenDoc))) == Right brokenDoc)
  ]
  where
    brokenDoc = fst (must (apply (SetUd mA1 (Just udSample {udHead = HeadOf (MorphId 999)})) baseDoc))
    setHead m h = perform (SetUd m (Just udSample {udHead = h}))
    headError m h = errorOf . setHead m h
    isRight = either (const False) (const True)
    performAny c = either (Left . JournalEdit) Right . perform c
    replayed = replayJournal emptyDocument (renderJournal session2)
    replayParseLine1 = case replayJournal emptyDocument "bogus\n" of
      Left (ReplayParse 1 _) -> True
      _ -> False
