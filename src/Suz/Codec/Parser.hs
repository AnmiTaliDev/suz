-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec.Parser
  ( Parser (..)
  , failP
  , field
  , intP
  , wordP
  , counted
  ) where

import Control.Monad (ap, replicateM)
import Data.Bifunctor (first)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Text.Read (readMaybe)

-- | Reads the tab-separated fields of one line, in order.
newtype Parser a = Parser {runParser :: [Text] -> Either String (a, [Text])}

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (first f) . p)

instance Applicative Parser where
  pure x = Parser (\fs -> Right (x, fs))
  (<*>) = ap

instance Monad Parser where
  Parser p >>= k = Parser $ \fs -> case p fs of
    Left e -> Left e
    Right (a, rest) -> runParser (k a) rest

-- | Fails with a message.
failP :: String -> Parser a
failP msg = Parser (const (Left msg))

-- | Takes the next field.
field :: Parser Text
field = Parser $ \fs -> case fs of
  f : rest -> Right (f, rest)
  [] -> Left "unexpected end of line"

-- | Takes the next field as a decimal integer.
intP :: Parser Int
intP = field >>= \t -> maybe (failP ("bad number: " ++ T.unpack t)) pure (readMaybe (T.unpack t))

wordP :: Parser Word64
wordP = field >>= \t -> case readMaybe (T.unpack t) :: Maybe Integer of
  Just n | n >= 0 && n <= fromIntegral (maxBound :: Word64) -> pure (fromIntegral n)
  _ -> failP ("bad identifier: " ++ T.unpack t)

counted :: Parser a -> Parser [a]
counted p = do
  n <- intP
  if n < 0 then failP "negative count" else replicateM n p
