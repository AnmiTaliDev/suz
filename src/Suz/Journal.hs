-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Journal
  ( Entry (..)
  , Session (..)
  , JournalError (..)
  , ReplayError (..)
  , newSession
  , perform
  , undo
  , redo
  , history
  , renderJournal
  , replayJournal
  ) where

import Control.Monad (foldM)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec
import Suz.Command
import Suz.Model

-- | A command together with the command that reverts it.
data Entry = Entry
  { entryForward :: Command
  , entryInverse :: Command
  }
  deriving (Eq, Show)

-- | The most recent entry is first in both lists.
data Session = Session
  { sessionDocument :: Document
  , sessionDone :: [Entry]
  , sessionUndone :: [Entry]
  }
  deriving (Eq, Show)

-- | Why 'undo' or 'redo' failed: there is nothing to revert or repeat, or the
-- stored command no longer applies.
data JournalError
  = NothingToUndo
  | NothingToRedo
  | JournalEdit EditError
  deriving (Eq, Show)

-- | Line number (from 1) and the problem found there.
data ReplayError
  = ReplayParse Int String
  | ReplayEdit Int EditError
  deriving (Eq, Show)

-- | Session over the given document with an empty journal.
newSession :: Document -> Session
newSession doc = Session doc [] []

-- | Applies a command and records it. The redo stack is discarded.
perform :: Command -> Session -> Either EditError Session
perform command session = do
  (doc, inverse) <- apply command (sessionDocument session)
  pure
    session
      { sessionDocument = doc
      , sessionDone = Entry command inverse : sessionDone session
      , sessionUndone = []
      }

-- | Reverts the most recent recorded command.
undo :: Session -> Either JournalError Session
undo session = case sessionDone session of
  [] -> Left NothingToUndo
  entry : rest -> do
    (doc, _) <- first JournalEdit (apply (entryInverse entry) (sessionDocument session))
    pure session {sessionDocument = doc, sessionDone = rest, sessionUndone = entry : sessionUndone session}

-- | Reapplies the most recently undone command.
redo :: Session -> Either JournalError Session
redo session = case sessionUndone session of
  [] -> Left NothingToRedo
  entry : rest -> do
    (doc, _) <- first JournalEdit (apply (entryForward entry) (sessionDocument session))
    pure session {sessionDocument = doc, sessionDone = entry : sessionDone session, sessionUndone = rest}

-- | Commands that produced the current document, oldest first.
history :: Session -> [Command]
history = reverse . map entryForward . sessionDone

-- | Text form of 'history', one command per line. Undone commands are not
-- included.
renderJournal :: Session -> Text
renderJournal = T.unlines . map encodeCommand . history

-- | Rebuilds a session by applying the commands of a rendered journal to the
-- given document. Blank lines are skipped.
replayJournal :: Document -> Text -> Either ReplayError Session
replayJournal doc text = foldM step (newSession doc) (zip [1 ..] (T.lines text))
  where
    step session (n, line)
      | T.null (T.strip line) = Right session
      | otherwise = do
          command <- first (ReplayParse n) (decodeCommand line)
          first (ReplayEdit n) (perform command session)
