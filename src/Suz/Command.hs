-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Command
  ( Command (..)
  , EditError (..)
  , apply
  ) where

import Control.Monad (foldM, unless)
import qualified Data.Text as T
import Suz.Command.Edit
import Suz.Command.Types
import Suz.Model

-- | Applies a command to a document. On success returns the new document and
-- the command that undoes the change. Inserted nodes carry their own
-- identifiers, which must not be in use. A 'Batch' is applied in order and
-- fails as a whole. 'SplitMorph' keeps the gloss and the UD annotation on the
-- first part and leaves the new part without them. 'MergeMorph' joins a morph
-- with the next one in the same token and keeps the gloss and the UD
-- annotation of the first. 'SplitToken' divides the morphs
-- at the first index and the surface form at the second, both may be 0 or the
-- full length, and gives the second part the new identifier. 'MergeToken'
-- joins a token with the next one in the same segment and keeps the
-- identifier of the first. 'MoveToken' puts the token at a position of the
-- given segment, counted after the token has been taken out.
apply :: Command -> Document -> Either EditError (Document, Command)
apply command doc = case command of
  InsertSegment i seg -> do
    checkFresh doc (segmentNodeIds seg)
    segs <- insertAt i seg (documentSegments doc)
    pure
      ( claimIds (segmentNodeIds seg) doc {documentSegments = segs}
      , RemoveSegment (segmentId seg)
      )
  RemoveSegment sid -> do
    (i, seg) <- needSegment sid doc
    pure (doc {documentSegments = removeAt i (documentSegments doc)}, InsertSegment i seg)
  InsertToken sid i tok -> do
    (_, seg) <- needSegment sid doc
    checkFresh doc (tokenNodeIds tok)
    toks <- insertAt i tok (segmentTokens seg)
    pure
      ( claimIds (tokenNodeIds tok) (replaceSegment seg {segmentTokens = toks} doc)
      , RemoveToken (tokenId tok)
      )
  RemoveToken tid -> do
    (seg, i, tok) <- needToken tid doc
    pure
      ( replaceSegment seg {segmentTokens = removeAt i (segmentTokens seg)} doc
      , InsertToken (segmentId seg) i tok
      )
  SetSurface tid surface -> do
    (_, _, tok) <- needToken tid doc
    pure (replaceToken tok {tokenSurface = surface} doc, SetSurface tid (tokenSurface tok))
  SplitToken tid k j newId@(TokenId n) -> do
    (seg, i, tok) <- needToken tid doc
    unless
      (k >= 0 && k <= length (tokenMorphs tok) && j >= 0 && j <= T.length (tokenSurface tok))
      (Left (BadTokenSplit tid k j))
    checkFresh doc [n]
    let (surfaceA, surfaceB) = T.splitAt j (tokenSurface tok)
        (morphsA, morphsB) = splitAt k (tokenMorphs tok)
        parts = [tok {tokenSurface = surfaceA, tokenMorphs = morphsA}, Token newId surfaceB morphsB]
        toks = segmentTokens seg
    pure
      ( claimIds [n] (replaceSegment seg {segmentTokens = take i toks ++ parts ++ drop (i + 1) toks} doc)
      , MergeToken tid
      )
  MergeToken tid -> do
    (seg, i, tok) <- needToken tid doc
    let toks = segmentTokens seg
    next <- case drop (i + 1) toks of
      t : _ -> Right t
      [] -> Left (NoNextToken tid)
    let merged =
          tok
            { tokenSurface = tokenSurface tok <> tokenSurface next
            , tokenMorphs = tokenMorphs tok ++ tokenMorphs next
            }
    pure
      ( replaceSegment seg {segmentTokens = take i toks ++ [merged] ++ drop (i + 2) toks} doc
      , SplitToken tid (length (tokenMorphs tok)) (T.length (tokenSurface tok)) (tokenId next)
      )
  MoveToken tid sid i -> do
    (src, j, tok) <- needToken tid doc
    let removed = replaceSegment src {segmentTokens = removeAt j (segmentTokens src)} doc
    (_, dst) <- needSegment sid removed
    toks <- insertAt i tok (segmentTokens dst)
    pure (replaceSegment dst {segmentTokens = toks} removed, MoveToken tid (segmentId src) j)
  InsertMorph tid i morph@(Morph (MorphId n) _ _ _) -> do
    (_, _, tok) <- needToken tid doc
    checkFresh doc [n]
    ms <- insertAt i morph (tokenMorphs tok)
    pure
      ( claimIds [n] (replaceToken tok {tokenMorphs = ms} doc)
      , RemoveMorph (morphId morph)
      )
  RemoveMorph mid -> do
    (tok, i, morph) <- needMorph mid doc
    pure
      ( replaceToken tok {tokenMorphs = removeAt i (tokenMorphs tok)} doc
      , InsertMorph (tokenId tok) i morph
      )
  SetForm mid form -> do
    (_, _, morph) <- needMorph mid doc
    pure (replaceMorph morph {morphForm = form} doc, SetForm mid (morphForm morph))
  SetGloss mid gloss -> do
    (_, _, morph) <- needMorph mid doc
    pure (replaceMorph morph {morphGloss = gloss} doc, SetGloss mid (morphGloss morph))
  SetUd mid ud -> do
    (_, _, morph) <- needMorph mid doc
    pure (replaceMorph morph {morphUd = ud} doc, SetUd mid (morphUd morph))
  SplitMorph mid k newId@(MorphId n) -> do
    (tok, i, morph) <- needMorph mid doc
    unless (k > 0 && k < T.length (morphForm morph)) (Left (BadSplitPoint mid k))
    checkFresh doc [n]
    let (before, after) = T.splitAt k (morphForm morph)
        ms = tokenMorphs tok
        parts = [morph {morphForm = before}, Morph newId after Nothing Nothing]
    pure
      ( claimIds [n] (replaceToken tok {tokenMorphs = take i ms ++ parts ++ drop (i + 1) ms} doc)
      , MergeMorph mid
      )
  MergeMorph mid -> do
    (tok, i, morph) <- needMorph mid doc
    let ms = tokenMorphs tok
    next <- case drop (i + 1) ms of
      n : _ -> Right n
      [] -> Left (NoNextMorph mid)
    let merged = morph {morphForm = morphForm morph <> morphForm next}
    pure
      ( replaceToken tok {tokenMorphs = take i ms ++ [merged] ++ drop (i + 2) ms} doc
      , Batch [InsertMorph (tokenId tok) (i + 1) next, SetForm mid (morphForm morph)]
      )
  Batch commands -> do
    (doc', inverses) <- foldM step (doc, []) commands
    pure (doc', Batch inverses)
  where
    step (d, inverses) c = do
      (d', inverse) <- apply c d
      pure (d', inverse : inverses)
