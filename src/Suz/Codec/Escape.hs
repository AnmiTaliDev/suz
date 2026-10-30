-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec.Escape
  ( escape
  , unescape
  , encodeLexical
  , decodeLexical
  ) where

import Data.Text (Text)
import qualified Data.Text as T

-- | Escapes backslash, tab, newline and carriage return so the text can be
-- used as a tab-separated field.
escape :: Text -> Text
escape = T.concatMap f
  where
    f '\\' = "\\\\"
    f '\t' = "\\t"
    f '\n' = "\\n"
    f '\r' = "\\r"
    f ch = T.singleton ch

-- | Inverse of 'escape'. Fails on an unknown or dangling escape.
unescape :: Text -> Either String Text
unescape =fmap T.pack . go . T.unpack
  where
    go ('\\' : ch : rest) = case ch of
      '\\' -> ('\\' :) <$> go rest
      't' -> ('\t' :) <$> go rest
      'n' -> ('\n' :) <$> go rest
      'r' -> ('\r' :) <$> go rest
      _ -> Left "unknown escape"
    go "\\" = Left "dangling escape"
    go (ch : rest) = (ch :) <$> go rest
    go [] = Right []

-- | Field form of the lexical part of a gloss: an empty part is written as
-- @-@, a literal @-@ as @\\-@, any other text is escaped with 'escape'.
encodeLexical :: Text -> Text
encodeLexical "" = "-"
encodeLexical "-" = "\\-"
encodeLexical t = escape t

-- | Inverse of 'encodeLexical'.
decodeLexical :: Text -> Either String Text
decodeLexical "-" = Right ""
decodeLexical "\\-" = Right "-"
decodeLexical t = unescape t
