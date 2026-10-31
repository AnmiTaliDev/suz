-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Codec.Ud
  ( udToFields
  , udFromFields
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz.Codec.Escape (decodeLexical, encodeLexical)
import Suz.Model
import Text.Read (readMaybe)

udToFields :: UdFields -> [Text]
udToFields u =
  [ encodeLexical (udLemma u)
  , encodeLexical (udUpos u)
  , encodeLexical (udXpos u)
  , encodeLexical (udFeats u)
  , headField (udHead u)
  , encodeLexical (udDeprel u)
  , encodeLexical (udDeps u)
  , encodeLexical (udMisc u)
  ]
  where
    headField NoHead = "-"
    headField RootHead = "root"
    headField (HeadOf (MorphId n)) = T.pack (show n)

udFromFields :: [Text] -> Either String UdFields
udFromFields [lemma, upos, xpos, feats, hd, deprel, deps, misc] =
  UdFields
    <$> decodeLexical lemma
    <*> decodeLexical upos
    <*> decodeLexical xpos
    <*> decodeLexical feats
    <*> headOf hd
    <*> decodeLexical deprel
    <*> decodeLexical deps
    <*> decodeLexical misc
  where
    headOf "-" = Right NoHead
    headOf "root" = Right RootHead
    headOf t = case readMaybe (T.unpack t) :: Maybe Integer of
      Just n | n >= 0 && n <= fromIntegral (maxBound :: Word64) -> Right (HeadOf (MorphId (fromIntegral n)))
      _ -> Left ("bad head: " ++ T.unpack t)
udFromFields _ = Left "expected 8 UD fields"
