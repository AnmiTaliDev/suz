-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Handle
  ( newHandle
  , deref
  , freeHandle
  , newUtf8
  , peekText
  , failCode
  , failHandle
  , lineError
  ) where

import Control.Monad (unless)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BU
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (free, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import Foreign.StablePtr (castPtrToStablePtr, castStablePtrToPtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (poke, pokeByteOff)

foreign export ccall "suz_free" freeString :: Ptr () -> IO ()

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
