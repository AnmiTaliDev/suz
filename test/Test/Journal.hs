-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Journal (checks) where

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
  ]
  where
    performAny c = either (Left . JournalEdit) Right . perform c
    replayed = replayJournal emptyDocument (renderJournal session2)
    replayParseLine1 = case replayJournal emptyDocument "bogus\n" of
      Left (ReplayParse 1 _) -> True
      _ -> False
