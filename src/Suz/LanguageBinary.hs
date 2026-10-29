-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.LanguageBinary
  ( encodeLanguage
  , decodeLanguage
  ) where

import Control.Monad (ap, replicateM, unless)
import Data.Bifunctor (first)
import Data.Bits (shiftL, (.|.))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)
import GHC.Float (castWord64ToDouble)
import Suz.Language
import Suz.Model

formatVersion :: Int
formatVersion = 2

-- | Binary form of a language. All integers are little-endian. The layout is
-- the magic @SUZL@, the format version (u16), the language name, the tags,
-- then the forms, then the context entries. A string is a u32 byte length
-- followed by UTF-8 bytes. A list is a u32 count followed by its items. A form
-- is its string, then a list of candidates, each a weight (IEEE 754 binary64),
-- the lexical part and a list of tags. A context entry is the previous form,
-- the form and a list of candidates. Tags, forms and context entries are
-- sorted and candidates go from the heaviest to the lightest, so equal
-- languages give equal bytes.
encodeLanguage :: Language -> ByteString
encodeLanguage lang = BL.toStrict (B.toLazyByteString body)
  where
    body =
      B.byteString "SUZL"
        <> B.word16LE (fromIntegral formatVersion)
        <> string (languageName lang)
        <> list string (Set.toList (languageTags lang))
        <> list (formBuilder lang) (Map.keys (languageWeights lang))
        <> list (contextBuilder lang) (Map.keys (languageContext lang))

-- | Inverse of 'encodeLanguage'. Reads format versions 1 (no context entries)
-- and 2. Fails on a wrong magic, an unsupported version, truncated or trailing
-- data, invalid UTF-8, and non-finite weights.
decodeLanguage :: ByteString -> Either String Language
decodeLanguage bytes = do
  (lang, rest) <- runReader languageR bytes
  unless (BS.null rest) (Left "trailing data")
  pure lang

string :: Text -> Builder
string t = B.word32LE (fromIntegral (BS.length b)) <> B.byteString b
  where
    b = TE.encodeUtf8 t

list :: (a -> Builder) -> [a] -> Builder
list f xs = B.word32LE (fromIntegral (length xs)) <> mconcat (map f xs)

formBuilder :: Language -> Text -> Builder
formBuilder lang form = string form <> list candidateBuilder (candidates lang form)

contextBuilder :: Language -> (Text, Text) -> Builder
contextBuilder lang (prev, form) =
  string prev <> string form <> list candidateBuilder (contextCandidates lang prev form)

candidateBuilder :: (Gloss, Double) -> Builder
candidateBuilder (Gloss lexical tags, w) = B.doubleLE w <> string lexical <> list string tags

newtype Reader a = Reader {runReader :: ByteString -> Either String (a, ByteString)}

instance Functor Reader where
  fmap f (Reader p) = Reader (fmap (first f) . p)

instance Applicative Reader where
  pure x = Reader (\b -> Right (x, b))
  (<*>) = ap

instance Monad Reader where
  Reader p >>= k = Reader $ \b -> case p b of
    Left e -> Left e
    Right (a, rest) -> runReader (k a) rest

failR :: String -> Reader a
failR msg = Reader (const (Left msg))

takeBytes :: Int -> Reader ByteString
takeBytes n = Reader $ \b ->
  if BS.length b < n then Left "unexpected end of data" else Right (BS.splitAt n b)

unsigned :: Int -> Reader Word64
unsigned n = BS.foldr' (\byte acc -> acc `shiftL` 8 .|. fromIntegral byte) 0 <$> takeBytes n

textR :: Reader Text
textR = do
  len <- unsigned 4
  bytes <- takeBytes (fromIntegral len)
  either (failR . show) pure (TE.decodeUtf8' bytes)

listR :: Reader a -> Reader [a]
listR item = unsigned 4 >>= \n -> replicateM (fromIntegral n) item

weightR :: Reader Double
weightR = do
  w <- castWord64ToDouble <$> unsigned 8
  if isNaN w || isInfinite w then failR "non-finite weight" else pure w

languageR :: Reader Language
languageR = do
  magic <- takeBytes 4
  unless (magic == "SUZL") (failR "bad magic")
  version <- unsigned 2
  unless (version `elem` [1, 2]) (failR ("unsupported version: " ++ show version))
  name <- textR
  tags <- listR textR
  entries <- concat <$> listR formR
  contexts <- if version >= 2 then concat <$> listR contextR else pure []
  pure (withContext contexts (mkLanguage name tags entries))

formR :: Reader [(Text, Gloss, Double)]
formR = do
  form <- textR
  map (\(g, w) -> (form, g, w)) <$> listR candidateR

contextR :: Reader [(Text, Text, Gloss, Double)]
contextR = do
  prev <- textR
  form <- textR
  map (\(g, w) -> (prev, form, g, w)) <$> listR candidateR

candidateR :: Reader (Gloss, Double)
candidateR = do
  w <- weightR
  lexical <- textR
  tags <- listR textR
  pure (Gloss lexical tags, w)
