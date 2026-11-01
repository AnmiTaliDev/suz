-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec.Xml
  ( escapeXml
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Escapes text for an XML element or attribute value: @&@, @<@, @>@, @\"@ and
-- @'@ become entities, a tab or line break becomes a space and the other
-- characters XML 1.0 does not allow, the control characters and U+FFFE and
-- U+FFFF, become U+FFFD.
escapeXml :: Text -> Text
escapeXml = T.concatMap replace
  where
    replace c = case c of
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '"' -> "&quot;"
      '\'' -> "&apos;"
      '\t' -> " "
      '\n' -> " "
      '\r' -> " "
      _
        | c < ' ' || c == '\xFFFE' || c == '\xFFFF' -> "\xFFFD"
        | otherwise -> T.singleton c
