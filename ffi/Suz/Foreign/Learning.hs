-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Learning () where

import Data.Bifunctor (first)
import Data.IORef (IORef, readIORef, writeIORef)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Suz
import Suz.Foreign.Handle (deref, failCode, peekText)
import Suz.Foreign.Language (readLanguage)

foreign export ccall "suz_perform_learning" sessionPerformLearning :: Ptr () -> Ptr () -> CString -> Ptr CString -> IO CInt
foreign export ccall "suz_suggest_segmentation" suggestSegmentationC :: Ptr () -> Ptr () -> Ptr CString -> IO CInt
foreign export ccall "suz_suggest_glosses" suggestGlossesC :: Ptr () -> Ptr () -> Ptr CString -> IO CInt

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
