-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Session
  ( readSession
  ) where

import Data.Bifunctor (first)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Text as T
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Suz
import Suz.Foreign.Handle (at, deref, failCode, failHandle, freeHandle, lineError, newHandle, newUtf8, peekText)
import Suz.Foreign.Language (readLanguage)

foreign export ccall "suz_session_new" sessionNew :: IO (Ptr ())
foreign export ccall "suz_session_from_snapshot" sessionFromSnapshot :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_journal" sessionFromJournal :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_text" sessionFromText :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_text_with_language" sessionFromTextWithLanguage :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_toolbox" sessionFromToolbox :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_leipzig" sessionFromLeipzig :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_conllu" sessionFromConllu :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_json" sessionFromJson :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_free" sessionFree :: Ptr () -> IO ()
foreign export ccall "suz_next_id" sessionNextId :: Ptr () -> IO Word64
foreign export ccall "suz_perform" sessionPerform :: Ptr () -> CString -> Ptr CString -> IO CInt
foreign export ccall "suz_set_ud" sessionSetUd :: Ptr () -> CInt -> CInt -> CInt -> CString -> CString -> CString -> CString -> CInt -> Word64 -> CString -> CString -> CString -> Ptr CString -> IO CInt
foreign export ccall "suz_clear_ud" sessionClearUd :: Ptr () -> CInt -> CInt -> CInt -> Ptr CString -> IO CInt
foreign export ccall "suz_undo" sessionUndo :: Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_redo" sessionRedo :: Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_render" sessionRender :: Ptr () -> IO CString
foreign export ccall "suz_render_wrapped" sessionRenderWrapped :: Ptr () -> CInt -> IO CString
foreign export ccall "suz_snapshot" sessionSnapshot :: Ptr () -> IO CString
foreign export ccall "suz_journal" sessionJournal :: Ptr () -> IO CString
foreign export ccall "suz_toolbox" sessionToolbox :: Ptr () -> IO CString
foreign export ccall "suz_json" sessionJson :: Ptr () -> IO CString
foreign export ccall "suz_latex" sessionLatex :: Ptr () -> IO CString
foreign export ccall "suz_html" sessionHtml :: Ptr () -> IO CString
foreign export ccall "suz_elan" sessionElan :: Ptr () -> IO CString
foreign export ccall "suz_validate_ud" sessionValidateUd :: Ptr () -> IO CString
foreign export ccall "suz_conllu" sessionConllu :: Ptr () -> IO CString

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

sessionFromTextWithLanguage :: Ptr () -> CString -> Ptr CString -> IO (Ptr ())
sessionFromTextWithLanguage lh s errp = do
  lang <- readLanguage lh
  input <- peekText s
  case input of
    Left msg -> failHandle errp msg
    Right text -> newIORef (newSession (importTextWith lang text)) >>= newHandle

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

sessionFromJson :: CString -> Ptr CString -> IO (Ptr ())
sessionFromJson s errp = do
  input <- peekText s
  case input >>= parseJson of
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

morphAt :: Ptr () -> CInt -> CInt -> CInt -> IO (Maybe Morph)
morphAt h s t m = do
  doc <- sessionDocument <$> readSession h
  pure (at s (documentSegments doc) >>= at t . segmentTokens >>= at m . tokenMorphs)

sessionSetUd :: Ptr () -> CInt -> CInt -> CInt -> CString -> CString -> CString -> CString -> CInt -> Word64 -> CString -> CString -> CString -> Ptr CString -> IO CInt
sessionSetUd h s t m lemma upos xpos feats headKind headId deprel deps misc errp = do
  target <- morphAt h s t m
  texts <- traverse peekText [lemma, upos, xpos, feats, deprel, deps, misc]
  case (target, sequence texts, headOf headKind) of
    (Nothing, _, _) -> failCode errp "no such morph position"
    (_, Left msg, _) -> failCode errp msg
    (_, _, Nothing) -> failCode errp "bad head kind"
    (Just morph, Right [l, u, x, f, d, ds, mi], Just hd) ->
      update h errp (first show . perform (SetUd (morphId morph) (Just (UdFields l u x f hd d ds mi))))
    _ -> failCode errp "bad UD fields"
  where
    headOf 0 = Just NoHead
    headOf 1 = Just RootHead
    headOf 2 = Just (HeadOf (MorphId headId))
    headOf _ = Nothing

sessionClearUd :: Ptr () -> CInt -> CInt -> CInt -> Ptr CString -> IO CInt
sessionClearUd h s t m errp = do
  target <- morphAt h s t m
  case target of
    Nothing -> failCode errp "no such morph position"
    Just morph -> update h errp (first show . perform (SetUd (morphId morph) Nothing))

sessionUndo :: Ptr () -> Ptr CString -> IO CInt
sessionUndo h errp = update h errp (first show . undo)

sessionRedo :: Ptr () -> Ptr CString -> IO CInt
sessionRedo h errp = update h errp (first show . redo)

sessionRender :: Ptr () -> IO CString
sessionRender h = readSession h >>= newUtf8 . renderDocument . sessionDocument

sessionRenderWrapped :: Ptr () -> CInt -> IO CString
sessionRenderWrapped h width = readSession h >>= newUtf8 . renderDocumentWrapped (fromIntegral width) . sessionDocument

sessionSnapshot :: Ptr () -> IO CString
sessionSnapshot h = readSession h >>= newUtf8 . renderSnapshot . sessionDocument

sessionJournal :: Ptr () -> IO CString
sessionJournal h = readSession h >>= newUtf8 . renderJournal

sessionToolbox :: Ptr () -> IO CString
sessionToolbox h = readSession h >>= newUtf8 . renderToolbox . sessionDocument

sessionConllu :: Ptr () -> IO CString
sessionConllu h = readSession h >>= newUtf8 . renderConllu . sessionDocument

sessionHtml :: Ptr () -> IO CString
sessionHtml h = readSession h >>= newUtf8 . renderHtml . sessionDocument

sessionElan :: Ptr () -> IO CString
sessionElan h = readSession h >>= newUtf8 . renderElan . sessionDocument

sessionValidateUd :: Ptr () -> IO CString
sessionValidateUd h = readSession h >>= newUtf8 . T.unlines . map line . validateUd . sessionDocument
  where
    line (UdDanglingHead m x) = T.intercalate "\t" ["dangling", number m, number x]
    line (UdHeadOutsideSegment m x) = T.intercalate "\t" ["outside", number m, number x]
    line (UdHeadCycle m) = T.intercalate "\t" ["cycle", number m]
    number (MorphId n) = T.pack (show n)

sessionLatex :: Ptr () -> IO CString
sessionLatex h = readSession h >>= newUtf8 . renderLatex . sessionDocument

sessionJson :: Ptr () -> IO CString
sessionJson h = readSession h >>= newUtf8 . renderJson . sessionDocument
