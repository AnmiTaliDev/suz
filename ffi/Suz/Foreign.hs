-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign () where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (poke, pokeByteOff)
import Suz

foreign export ccall "suz_free" freeString :: Ptr () -> IO ()
foreign export ccall "suz_session_new" sessionNew :: IO (Ptr ())
foreign export ccall "suz_session_from_snapshot" sessionFromSnapshot :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_from_journal" sessionFromJournal :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_session_free" sessionFree :: Ptr () -> IO ()
foreign export ccall "suz_next_id" sessionNextId :: Ptr () -> IO Word64
foreign export ccall "suz_perform" sessionPerform :: Ptr () -> CString -> Ptr CString -> IO CInt
foreign export ccall "suz_undo" sessionUndo :: Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_redo" sessionRedo :: Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_render" sessionRender :: Ptr () -> IO CString
foreign export ccall "suz_snapshot" sessionSnapshot :: Ptr () -> IO CString
foreign export ccall "suz_journal" sessionJournal :: Ptr () -> IO CString
foreign export ccall "suz_language_from_text" languageFromText :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_language_from_binary" languageFromBinary :: Ptr Word8 -> CSize -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_language_free" languageFree :: Ptr () -> IO ()
foreign export ccall "suz_language_to_text" languageToText :: Ptr () -> IO CString
foreign export ccall "suz_language_to_binary" languageToBinary :: Ptr () -> Ptr CSize -> IO (Ptr Word8)
foreign export ccall "suz_perform_learning" sessionPerformLearning :: Ptr () -> Ptr () -> CString -> Ptr CString -> IO CInt
foreign export ccall "suz_suggest_segmentation" suggestSegmentationC :: Ptr () -> Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_suggest_glosses" suggestGlossesC :: Ptr () -> Ptr () -> Ptr CString -> IO CInt

newHandle :: a -> IO (Ptr ())
newHandle x = castStablePtrToPtr <$> newStablePtr x

deref :: Ptr () -> IO a
deref = deRefStablePtr . castPtrToStablePtr

freeHandle :: Ptr () -> IO ()
freeHandle = freeStablePtr . castPtrToStablePtr

newUtf8 :: Text -> IO CString
newUtf8 t = do
  let bytes = TE.encodeUtf8 t
  buffer <- mallocBytes (BS.length bytes + 1)
  BU.unsafeUseAsCStringLen bytes $ \(src, len) -> copyBytes buffer (castPtr src) len
  pokeByteOff buffer (BS.length bytes) (0 :: Word8)
  pure (castPtr buffer)

peekText :: CString -> IO (Either String Text)
peekText s = do
  bytes <- BS.packCString s
  pure (first (const "input is not valid UTF-8") (TE.decodeUtf8' bytes))

reportError :: Ptr CString -> String -> IO ()
reportError errp msg = unless (errp == nullPtr) (newUtf8 (T.pack msg) >>= poke errp)

failCode :: Ptr CString -> String -> IO CInt
failCode errp msg = reportError errp msg >> pure (-1)

failHandle :: Ptr CString -> String -> IO (Ptr ())
failHandle errp msg = reportError errp msg >> pure nullPtr

lineError :: (Int, String) -> String
lineError (n, msg) = "line " ++ show n ++ ": " ++ msg

freeString :: Ptr () -> IO ()
freeString = free

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

languageFromText :: CString -> Ptr CString -> IO (Ptr ())
languageFromText s errp = do
  input <- peekText s
  case input >>= first lineError . parseLanguage of
    Left msg -> failHandle errp msg
    Right lang -> newIORef lang >>= newHandle

languageFromBinary :: Ptr Word8 -> CSize -> Ptr CString -> IO (Ptr ())
languageFromBinary p n errp = do
  bytes <- BS.packCStringLen (castPtr p, fromIntegral n)
  case decodeLanguage bytes of
    Left msg -> failHandle errp msg
    Right lang -> newIORef lang >>= newHandle

languageFree :: Ptr () -> IO ()
languageFree = freeHandle

readLanguage :: Ptr () -> IO Language
readLanguage h = deref h >>= readIORef

languageToText :: Ptr () -> IO CString
languageToText h = readLanguage h >>= newUtf8 . renderLanguage

languageToBinary :: Ptr () -> Ptr CSize -> IO (Ptr Word8)
languageToBinary h sizep = do
  bytes <- encodeLanguage <$> readLanguage h
  buffer <- mallocBytes (max 1 (BS.length bytes))
  BU.unsafeUseAsCStringLen bytes $ \(src, len) -> copyBytes buffer (castPtr src) len
  poke sizep (fromIntegral (BS.length bytes))
  pure buffer

sessionPerformLearning :: Ptr () -> Ptr () -> CString -> Ptr CString -> IO CInt
sessionPerformLearning h lh line errp = do
  input <- peekText line
  case input of
    Left msg -> failCode errp msg
    Right text -> do
      ref <- deref h :: IO (IORef Session)
      langRef <- deref lh :: IO (IORef Language)
      session <- readIORef ref
      lang <- readIORef langRef
      case first ("bad command: " ++) (decodeCommand text) of
        Left msg -> failCode errp msg
        Right command -> case performLearning command lang session of
          Left e -> failCode errp (show e)
          Right (session', lang') -> writeIORef ref session' >> writeIORef langRef lang' >> pure 0

suggestWith :: (Language -> Document -> [Command]) -> Ptr () -> Ptr () -> Ptr CString -> IO CInt
suggestWith suggestions h lh errp = do
  ref <- deref h :: IO (IORef Session)
  lang <- readLanguage lh
  session <- readIORef ref
  case suggestions lang (sessionDocument session) of
    [] -> pure 0
    commands -> case perform (Batch commands) session of
      Left e -> failCode errp (show e)
      Right session' -> writeIORef ref session' >> pure (fromIntegral (length commands))

suggestSegmentationC :: Ptr () -> Ptr () -> Ptr CString -> IO CInt
suggestSegmentationC = suggestWith suggestSegmentation

suggestGlossesC :: Ptr () -> Ptr () -> Ptr CString -> IO CInt
suggestGlossesC = suggestWith suggestGlosses
