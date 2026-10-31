-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Builtin () where

import Data.IORef (newIORef)
import qualified Data.Text as T
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Suz.Foreign.Handle (at, failHandle, newHandle, peekText, stringOut)
import Suz.Language
import Suz.Languages.Azerbaijani (azerbaijani)
import Suz.Languages.CrimeanTatar (crimeanTatar)
import Suz.Languages.English (english)
import Suz.Languages.German (german)
import Suz.Languages.Kazakh (kazakh)
import Suz.Languages.Kyrgyz (kyrgyz)
import Suz.Languages.Russian (russian)
import Suz.Languages.Spanish (spanish)
import Suz.Languages.Tatar (tatar)
import Suz.Languages.Turkish (turkish)
import Suz.Languages.Turkmen (turkmen)
import Suz.Languages.Uzbek (uzbek)

foreign export ccall "suz_builtin_language_count" builtinCount :: IO CInt
foreign export ccall "suz_builtin_language_name" builtinName :: CInt -> IO CString
foreign export ccall "suz_language_builtin" languageBuiltin :: CString -> Ptr CString -> IO (Ptr ())

builtins :: [Language]
builtins = [azerbaijani, crimeanTatar, english, german, kazakh, kyrgyz, russian, spanish, tatar, turkish, turkmen, uzbek]

builtinCount :: IO CInt
builtinCount = pure (fromIntegral (length builtins))

builtinName :: CInt -> IO CString
builtinName i = stringOut (languageName <$> at i builtins)

languageBuiltin :: CString -> Ptr CString -> IO (Ptr ())
languageBuiltin s errp = do
  input <- peekText s
  case input of
    Left msg -> failHandle errp msg
    Right name -> case filter ((== name) . languageName) builtins of
      lang : _ -> newIORef lang >>= newHandle
      [] -> failHandle errp ("unknown language: " ++ T.unpack name)
