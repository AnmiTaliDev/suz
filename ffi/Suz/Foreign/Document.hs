-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Foreign.Document () where

import Data.List (genericLength)
import Data.Word (Word64)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Suz
import Suz.Foreign.Handle (at, stringOut)
import Suz.Foreign.Session (readSession)

foreign export ccall "suz_segment_count" segmentCount :: Ptr () -> IO CInt
foreign export ccall "suz_token_count" tokenCount :: Ptr () -> CInt -> IO CInt
foreign export ccall "suz_morph_count" morphCount :: Ptr () -> CInt -> CInt -> IO CInt
foreign export ccall "suz_segment_id" segmentIdOf :: Ptr () -> CInt -> Ptr Word64 -> IO CInt
foreign export ccall "suz_token_id" tokenIdOf :: Ptr () -> CInt -> CInt -> Ptr Word64 -> IO CInt
foreign export ccall "suz_morph_id" morphIdOf :: Ptr () -> CInt -> CInt -> CInt -> Ptr Word64 -> IO CInt
foreign export ccall "suz_token_surface" tokenSurface' :: Ptr () -> CInt -> CInt -> IO CString
foreign export ccall "suz_morph_form" morphForm' :: Ptr () -> CInt -> CInt -> CInt -> IO CString
foreign export ccall "suz_morph_glossed" morphGlossed :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign export ccall "suz_morph_lexical" morphLexical :: Ptr () -> CInt -> CInt -> CInt -> IO CString
foreign export ccall "suz_morph_tag_count" morphTagCount :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign export ccall "suz_morph_tag" morphTag :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CString
foreign export ccall "suz_morph_has_ud" morphHasUd :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
foreign export ccall "suz_morph_ud_text" morphUdText :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CString
foreign export ccall "suz_morph_ud_head" morphUdHead :: Ptr () -> CInt -> CInt -> CInt -> Ptr Word64 -> IO CInt

segmentAt :: Ptr () -> CInt -> IO (Maybe Segment)
segmentAt h s = at s . documentSegments . sessionDocument <$> readSession h

tokenAt :: Ptr () -> CInt -> CInt -> IO (Maybe Token)
tokenAt h s t = (>>= at t . segmentTokens) <$> segmentAt h s

morphAt :: Ptr () -> CInt -> CInt -> CInt -> IO (Maybe Morph)
morphAt h s t m = (>>= at m . tokenMorphs) <$> tokenAt h s t

count :: (a -> [b]) -> Maybe a -> CInt
count f = maybe (-1) (genericLength . f)

idOut :: Ptr Word64 -> Maybe Word64 -> IO CInt
idOut p = maybe (pure (-1)) (\n -> poke p n >> pure 0)

segmentCount :: Ptr () -> IO CInt
segmentCount h = genericLength . documentSegments . sessionDocument <$> readSession h

tokenCount :: Ptr () -> CInt -> IO CInt
tokenCount h s = count segmentTokens <$> segmentAt h s

morphCount :: Ptr () -> CInt -> CInt -> IO CInt
morphCount h s t = count tokenMorphs <$> tokenAt h s t

segmentIdOf :: Ptr () -> CInt -> Ptr Word64 -> IO CInt
segmentIdOf h s p = segmentAt h s >>= idOut p . fmap (\x -> let SegmentId n = segmentId x in n)

tokenIdOf :: Ptr () -> CInt -> CInt -> Ptr Word64 -> IO CInt
tokenIdOf h s t p = tokenAt h s t >>= idOut p . fmap (\x -> let TokenId n = tokenId x in n)

morphIdOf :: Ptr () -> CInt -> CInt -> CInt -> Ptr Word64 -> IO CInt
morphIdOf h s t m p = morphAt h s t m >>= idOut p . fmap (\x -> let MorphId n = morphId x in n)

tokenSurface' :: Ptr () -> CInt -> CInt -> IO CString
tokenSurface' h s t = tokenAt h s t >>= stringOut . fmap tokenSurface

morphForm' :: Ptr () -> CInt -> CInt -> CInt -> IO CString
morphForm' h s t m = morphAt h s t m >>= stringOut . fmap morphForm

morphGlossed :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
morphGlossed h s t m = maybe (-1) (maybe 0 (const 1) . morphGloss) <$> morphAt h s t m

morphLexical :: Ptr () -> CInt -> CInt -> CInt -> IO CString
morphLexical h s t m = morphAt h s t m >>= stringOut . fmap glossLexical . (>>= morphGloss)

morphTagCount :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
morphTagCount h s t m = maybe (-1) (maybe 0 (genericLength . glossTags) . morphGloss) <$> morphAt h s t m

morphTag :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CString
morphTag h s t m i = morphAt h s t m >>= stringOut . (>>= at i . glossTags) . (>>= morphGloss)

morphHasUd :: Ptr () -> CInt -> CInt -> CInt -> IO CInt
morphHasUd h s t m = maybe (-1) (maybe 0 (const 1) . morphUd) <$> morphAt h s t m

morphUdText :: Ptr () -> CInt -> CInt -> CInt -> CInt -> IO CString
morphUdText h s t m field = morphAt h s t m >>= stringOut . (>>= \u -> at field [udLemma u, udUpos u, udXpos u, udFeats u, udDeprel u, udDeps u, udMisc u]) . (>>= morphUd)

morphUdHead :: Ptr () -> CInt -> CInt -> CInt -> Ptr Word64 -> IO CInt
morphUdHead h s t m p = morphAt h s t m >>= maybe (pure (-1)) (maybe (pure (-1)) headOut . morphUd)
  where
    headOut u = case udHead u of
      NoHead -> pure 0
      RootHead -> pure 1
      HeadOf (MorphId n) -> poke p n >> pure 2
