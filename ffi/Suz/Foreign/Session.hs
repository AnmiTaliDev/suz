-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Session
  ( readSession
  ) where

import Data.Bifunctor (first)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Suz
import Suz.Foreign.Handle (deref, failCode, failHandle, freeHandle, lineError, newHandle, newUtf8, peekText)
import Suz.Foreign.Language (readLanguage)

foreign export ccall "suz_session_new" sessionNew :: IO (Ptr ())
foreign export ccall "suz_session_from_snapshot" sessionFromSnapshot :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_journal" sessionFromJournal :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_text" sessionFromText :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_toolbox" sessionFromToolbox :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_leipzig" sessionFromLeipzig :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_conllu" sessionFromConllu :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_free" sessionFree :: Ptr () -> IO ()
foreign export ccall "suz_next_id" sessionNextId :: Ptr () -> IO Word64
foreign export ccall "suz_perform" sessionPerform :: Ptr () -> CString -> Ptr CString -> IO CInt
foreign export ccall "suz_undo" sessionUndo :: Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_redo" sessionRedo :: Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_render" sessionRender :: Ptr () -> IO CString
foreign export ccall "suz_snapshot" sessionSnapshot :: Ptr () -> IO CString
foreign export ccall "suz_journal" sessionJournal :: Ptr () -> IO CString
foreign export ccall "suz_toolbox" sessionToolbox :: Ptr () -> IO CString

sessionNew :: IO (Ptr ())
sessionNew = newIORef (newSession emptyDocument) >>= newHandle

sessionFromSnapshot :: CString -> Ptr CString -> IO (Ptr ())
sessionFromSnapshot s errp = do
  input <- peekText s
  case input >>= first lineError . parseSnapshot of
    Left msg -> failHandle errp msg
    Right doc -> newIORef (newSession doc) >>= newHandle

sessionFromJournal :: CString -> Ptr CString -> IO (Ptr ())
sessionFromJournal s errp = do
  input <- peekText s
  case input >>= first show . replayJournal emptyDocument of
    Left msg -> failHandle errp msg
    Right session -> newIORef session >>= newHandle

sessionFromText :: CString -> Ptr CString -> IO (Ptr ())
sessionFromText s errp = do
  input <- peekText s
  case input of
    Left msg -> failHandle errp msg
    Right text -> newIORef (newSession (importText text)) >>= newHandle

sessionFromToolbox :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
sessionFromToolbox lh s errp = do
  lang <- readLanguage lh
  input <- peekText s
  case input >>= first lineError . parseToolbox lang of
    Left msg -> failHandle errp msg
    Right doc -> newIORef (newSession doc) >>= newHandle

sessionFromLeipzig :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
sessionFromLeipzig lh s errp = do
  lang <- readLanguage lh
  input <- peekText s
  case input >>= first lineError . parseLeipzig lang of
    Left msg -> failHandle errp msg
    Right doc -> newIORef (newSession doc) >>= newHandle

sessionFromConllu :: CString -> Ptr CString -> IO (Ptr ())
sessionFromConllu s errp = do
  input <- peekText s
  case input >>= first lineError . parseConllu of
    Left msg -> failHandle errp msg
    Right doc -> newIORef (newSession doc) >>= newHandle

sessionFree :: Ptr () -> IO ()
sessionFree = freeHandle

readSession :: Ptr () -> IO Session
readSession h = deref h >>= readIORef

sessionNextId :: Ptr () -> IO Word64
sessionNextId h = nextFreeId . sessionDocument <$> readSession h

update :: Ptr () -> Ptr CString -> (Session -> Either String Session) -> IO CInt
update h errp f = do
  ref <- deref h :: IO (IORef Session)
  session <- readIORef ref
  case f session of
    Left msg -> failCode errp msg
    Right session' -> writeIORef ref session' >> pure 0

sessionPerform :: Ptr () -> CString -> Ptr CString -> IO CInt
sessionPerform h line errp = do
  input <- peekText line
  case input of
    Left msg -> failCode errp msg
    Right text -> update h errp $ \session -> do
      command <- first ("bad command: " ++) (decodeCommand text)
      first show (perform command session)

sessionUndo :: Ptr () -> Ptr CString -> IO CInt
sessionUndo h errp = update h errp (first show . undo)

sessionRedo :: Ptr () -> Ptr CString -> IO CInt
sessionRedo h errp = update h errp (first show . redo)

sessionRender :: Ptr () -> IO CString
sessionRender h = readSession h >>= newUtf8 . renderDocument . sessionDocument

sessionSnapshot :: Ptr () -> IO CString
sessionSnapshot h = readSession h >>= newUtf8 . renderSnapshot . sessionDocument

sessionJournal :: Ptr () -> IO CString
sessionJournal h = readSession h >>= newUtf8 . renderJournal

sessionToolbox :: Ptr () -> IO CString
sessionToolbox h = readSession h >>= newUtf8 . renderToolbox . sessionDocument
