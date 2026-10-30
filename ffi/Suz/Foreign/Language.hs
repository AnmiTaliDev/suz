-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Language
  ( readLanguage
  ) where

import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.IORef (newIORef, readIORef)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize (..))
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (poke)
import Suz
import Suz.Foreign.Handle (deref, failHandle, freeHandle, lineError, newHandle, newUtf8, peekText)

foreign export ccall "suz_language_from_text" languageFromText :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_language_from_binary" languageFromBinary :: Ptr Word8 -> CSize -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_language_free" languageFree :: Ptr () -> IO ()
foreign export ccall "suz_language_to_text" languageToText :: Ptr () -> IO CString
foreign export ccall "suz_language_to_binary" languageToBinary :: Ptr () -> Ptr CSize -> IO (Ptr Word8)

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
