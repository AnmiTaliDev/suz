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
import Foreign.C.Types (CDouble (..), CInt (..), CSize (..))
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (poke)
import Suz
import Suz.Foreign.Handle (at, deref, failHandle, freeHandle, lineError, newHandle, newUtf8, peekText, stringOut)

foreign export ccall "suz_language_from_text" languageFromText :: CString -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_language_from_binary" languageFromBinary :: Ptr Word8 -> CSize -> Ptr CString -> IO (Ptr ())
foreign export ccall "suz_language_free" languageFree :: Ptr () -> IO ()
foreign export ccall "suz_language_to_text" languageToText :: Ptr () -> IO CString
foreign export ccall "suz_language_to_binary" languageToBinary :: Ptr () -> Ptr CSize -> IO (Ptr Word8)
foreign export ccall "suz_language_name" languageNameOf :: Ptr () -> IO CString
foreign export ccall "suz_language_tag_count" languageTagCount :: Ptr () -> IO CInt
foreign export ccall "suz_language_tag" languageTag :: Ptr () -> CInt -> IO CString
foreign export ccall "suz_language_candidate_count" languageCandidateCount :: Ptr () -> CString -> IO CInt
foreign export ccall "suz_language_candidate_weight" languageCandidateWeight :: Ptr () -> CString -> CInt -> Ptr CDouble -> IO CInt
foreign export ccall "suz_language_candidate_lexical" languageCandidateLexical :: Ptr () -> CString -> CInt -> IO CString
foreign export ccall "suz_language_candidate_tag_count" languageCandidateTagCount :: Ptr () -> CString -> CInt -> IO CInt
foreign export ccall "suz_language_candidate_tag" languageCandidateTag :: Ptr () -> CString -> CInt -> CInt -> IO CString

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

languageNameOf :: Ptr () -> IO CString
languageNameOf h = readLanguage h >>= newUtf8 . languageName

languageTagCount :: Ptr () -> IO CInt
languageTagCount h = fromIntegral . length . languageTags <$> readLanguage h

languageTag :: Ptr () -> CInt -> IO CString
languageTag h i = readLanguage h >>= stringOut . at i . foldr (:) [] . languageTags

languageCandidateCount :: Ptr () -> CString -> IO CInt
languageCandidateCount h form = do
  lang <- readLanguage h
  text <- peekText form
  pure (either (const (-1)) (fromIntegral . length . candidates lang) text)

candidateAt :: Ptr () -> CString -> CInt -> IO (Maybe (Gloss, Double))
candidateAt h form i = do
  lang <- readLanguage h
  text <- peekText form
  pure (either (const Nothing) (\t -> at i (candidates lang t)) text)

languageCandidateWeight :: Ptr () -> CString -> CInt -> Ptr CDouble -> IO CInt
languageCandidateWeight h form i p =
  candidateAt h form i >>= maybe (pure (-1)) (\(_, w) -> poke p (realToFrac w) >> pure 0)

languageCandidateLexical :: Ptr () -> CString -> CInt -> IO CString
languageCandidateLexical h form i = candidateAt h form i >>= stringOut . fmap (glossLexical . fst)

languageCandidateTagCount :: Ptr () -> CString -> CInt -> IO CInt
languageCandidateTagCount h form i = maybe (-1) (fromIntegral . length . glossTags . fst) <$> candidateAt h form i

languageCandidateTag :: Ptr () -> CString -> CInt -> CInt -> IO CString
languageCandidateTag h form i t = candidateAt h form i >>= stringOut . (>>= at t . glossTags . fst)
