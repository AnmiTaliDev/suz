-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Property (properties) where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.List (mapAccumL, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T
import Test.QuickCheck
import Suz

wild, tame :: String
wild = "аБ b\t\n\r\\-._?\x00a0;#"
tame = "абвАБВ"

tagNames :: [Text]
tagNames = ["PL", "ACC", "PST"]

textOf :: String -> Int -> Int -> Gen Text
textOf alphabet lo hi = do
  n <- choose (lo, hi)
  T.pack <$> vectorOf n (elements alphabet)

smallList :: Int -> Gen a -> Gen [a]
smallList hi g = do
  n <- choose (0, hi)
  vectorOf n g

genGloss :: Gen Gloss
genGloss = Gloss <$> textOf wild 0 3 <*> smallList 2 (textOf wild 0 3)

genReadableGloss :: Gen Gloss
genReadableGloss = do
  tags <- smallList 2 (elements tagNames)
  lexical <- if null tags then textOf wild 1 3 else oneof [pure "", textOf wild 1 3]
  pure (Gloss lexical tags)

type Shape = [[(Text, [(Text, Maybe Gloss)])]]

assemble :: Shape -> Document
assemble segs = Document next segments
  where
    (next, segments) = mapAccumL segment 0 segs
    segment n toks = let (end, ts) = mapAccumL token (n + 1) toks in (end, Segment (SegmentId n) ts)
    token n (surface, ms) =
      ( n + 1 + fromIntegral (length ms)
      , Token (TokenId n) surface [Morph (MorphId i) f g Nothing | (i, (f, g)) <- zip [n + 1 ..] ms]
      )

genShape :: Gen Text -> Gen (Maybe Gloss) -> Int -> Int -> Gen Shape
genShape text gloss minTokens minMorphs = do
  n <- choose (0, 3)
  vectorOf n (do k <- choose (minTokens, 3); vectorOf k token)
  where
    token = do
      surface <- text
      n <- choose (minMorphs, 3)
      ms <- vectorOf n ((,) <$> text <*> gloss)
      pure (surface, ms)

genDocument :: Gen Document
genDocument = assemble <$> genShape (textOf wild 0 4) (oneof [pure Nothing, Just <$> genGloss]) 0 0

genToolboxDocument :: Gen Document
genToolboxDocument = assemble <$> genShape (textOf wild 1 4) (oneof [pure Nothing, Just <$> genReadableGloss]) 0 0

genLeipzigDocument :: Gen Document
genLeipzigDocument = assemble <$> genShape (textOf tame 1 4) (oneof [pure Nothing, Just <$> leipzigGloss]) 1 1
  where
    leipzigGloss = oneof [Gloss <$> textOf tame 1 3 <*> smallList 2 (elements tagNames), Gloss "" <$> ((:) <$> elements tagNames <*> smallList 1 (elements tagNames))]

genCommand :: Document -> Gen Command
genCommand d = oneof (map (>>= pure) gens)
  where
    segs = documentSegments d
    toks = [(segmentId s, t) | s <- segs, t <- segmentTokens s]
    morphs = [(tokenId t, m) | (_, t) <- toks, m <- tokenMorphs t]
    fresh = MorphId (nextFreeId d)
    forms = smallList 3 (textOf wild 0 3)
    specs = smallList 2 ((,) <$> textOf wild 0 3 <*> forms)
    maybeGloss = oneof [pure Nothing, Just <$> genGloss]
    always =
      [ InsertSegment <$> choose (0, length segs) <*> (buildSegment d <$> specs)
      ]
    withSegments =
      [ RemoveSegment . segmentId <$> elements segs
      , do
          s <- elements segs
          InsertToken (segmentId s) <$> choose (0, length (segmentTokens s)) <*> (buildToken d <$> textOf wild 0 3 <*> forms)
      ]
    withTokens =
      [ RemoveToken . tokenId . snd <$> elements toks
      , SetSurface . tokenId . snd <$> elements toks <*> textOf wild 0 3
      , MergeToken . tokenId . snd <$> elements toks
      , do
          (_, t) <- elements toks
          SplitToken (tokenId t) <$> choose (0, length (tokenMorphs t)) <*> choose (0, 3) <*> pure (TokenId (nextFreeId d))
      , do
          (_, t) <- elements toks
          (sid, _) <- elements toks
          MoveToken (tokenId t) sid <$> choose (0, 3)
      , do
          (_, t) <- elements toks
          InsertMorph (tokenId t) <$> choose (0, length (tokenMorphs t)) <*> (Morph fresh <$> textOf wild 0 3 <*> maybeGloss <*> pure Nothing)
      ]
    withMorphs =
      [ RemoveMorph . morphId . snd <$> elements morphs
      , SetForm . morphId . snd <$> elements morphs <*> textOf wild 0 3
      , SetGloss . morphId . snd <$> elements morphs <*> maybeGloss
      , MergeMorph . morphId . snd <$> elements morphs
      , do
          (_, m) <- elements morphs
          SplitMorph (morphId m) <$> choose (0, 4) <*> pure fresh
      ]
    gens = always ++ (if null segs then [] else withSegments) ++ (if null toks then [] else withTokens) ++ (if null morphs then [] else withMorphs)

genRun :: Int -> Document -> Gen (Document, [Command])
genRun 0 d = pure (d, [])
genRun k d = do
  c <- genCommand d
  case apply c d of
    Left _ -> genRun (k - 1) d
    Right (d', _) -> fmap (c :) <$> genRun (k - 1) d'

genScenario :: Gen (Document, Command)
genScenario = do
  d0 <- genDocument
  (d, _) <- genRun 3 d0
  c <- genCommand d
  pure (d, c)

genLanguage :: Gen Language
genLanguage = do
  name <- textOf wild 0 3
  tags <- smallList 3 (textOf wild 0 3)
  entries <- smallList 4 ((,,) <$> textOf wild 0 3 <*> genGloss <*> weight)
  contexts <- smallList 3 ((,,,) <$> textOf wild 0 2 <*> textOf wild 0 2 <*> genGloss <*> weight)
  punctuation <- smallList 3 (elements wild)
  folding <- elements [NoFolding, SimpleFolding, TurkicFolding]
  pure (withImportRules punctuation folding (withContext contexts (mkLanguage name tags entries)))
  where
    weight = (\n -> fromIntegral (n :: Int) / 8) <$> choose (-80, 80)

genRawText :: Gen Text
genRawText = T.intercalate "\n" <$> smallList 3 (T.unwords <$> smallList 4 (textOf "абв.,!«»-" 0 5))

genKnownGloss :: Gen Gloss
genKnownGloss = Gloss <$> textOf "xy" 0 2 <*> smallList 2 (elements tagNames)

genKnownLanguage :: Gen Language
genKnownLanguage = do
  tags <- sublistOf tagNames
  entries <- smallList 6 ((,,) <$> textOf "ab" 1 2 <*> genKnownGloss <*> weight)
  contexts <- smallList 3 ((,,,) <$> textOf "ab" 1 2 <*> textOf "ab" 1 2 <*> genKnownGloss <*> weight)
  pure (withContext contexts (mkLanguage "k" tags entries))
  where
    weight = (\n -> fromIntegral (n :: Int) / 2) <$> choose (-3, 6)

genKnownDocument :: Int -> Gen Document
genKnownDocument maxMorphs = assemble <$> shape
  where
    shape = do
      n <- choose (0, 3)
      vectorOf n (smallList 3 token)
    token = do
      k <- choose (1, maxMorphs)
      ms <- vectorOf k ((,) <$> textOf "ab" 1 4 <*> oneof [pure Nothing, Just <$> genKnownGloss])
      pure (T.concat (map fst ms), ms)

mutate :: Text -> Gen Text
mutate t
  | T.null t = pure t
  | otherwise = do
      k <- choose (0, T.length t - 1)
      c <- elements wild
      oneof [pure (T.take k t <> T.drop (k + 1) t), pure (T.take k t <> T.singleton c <> T.drop (k + 1) t), pure (T.take k t <> T.singleton c <> T.drop k t)]

allTokens :: Document -> [Token]
allTokens = concatMap segmentTokens . documentSegments

allMorphs :: Document -> [Morph]
allMorphs = concatMap tokenMorphs . concatMap segmentTokens . documentSegments

applyAll :: [Command] -> Document -> Either EditError Document
applyAll cs d = foldM (\acc c -> fst <$> apply c acc) d cs

weightOf :: Language -> Text -> Gloss -> Double
weightOf lang form g = maybe 0 id (lookup g (candidates lang form))

weightAfter :: Language -> Maybe Text -> Text -> Gloss -> Double
weightAfter lang prev form g = maybe 0 id (lookup g (candidatesAfter lang prev form))

properties :: [(String, Property)]
properties =
  [ ("snapshot round trip", forAll genDocument $ \d -> parseSnapshot (renderSnapshot d) == Right d)
  , ("toolbox round trip", forAll genToolboxDocument $ \d -> parseToolbox toolboxLanguage (renderToolbox d) == Right d)
  , ("leipzig round trip", forAll genLeipzigDocument $ \d -> parseLeipzig toolboxLanguage (renderDocument d) == Right d)
  , ("command codec round trip", forAll genScenario $ \(_, c) -> decodeCommand (encodeCommand c) == Right c)
  , ( "the inverse of a command restores the document"
    , forAll genScenario $ \(d, c) -> case apply c d of
        Left _ -> property Discard
        Right (d', inverse) -> fmap (documentSegments . fst) (apply inverse d') === Right (documentSegments d)
    )
  , ( "undo restores the document and redo repeats the command"
    , forAll genScenario $ \(d, c) -> case perform c (newSession d) of
        Left _ -> property Discard
        Right s -> fmap (documentSegments . sessionDocument) (undo s) === Right (documentSegments d) .&&. fmap sessionDocument (undo s >>= redo) === Right (sessionDocument s)
    )
  , ( "a batch gives the same document as its commands applied one by one"
    , forAll (genDocument >>= \d -> (,) d <$> genRun 4 d) $ \(d0, (d, cs)) ->
        fmap fst (apply (Batch cs) d0) === Right d
    )
  , ( "the inverse of a batch restores the document"
    , forAll (genDocument >>= \d -> (,) d <$> genRun 4 d) $ \(d0, (_, cs)) -> case apply (Batch cs) d0 of
        Left e -> counterexample (show e) False
        Right (d', inverse) -> fmap (documentSegments . fst) (apply inverse d') === Right (documentSegments d0)
    )
  , ( "a batch is one step of undo and redo"
    , forAll (genDocument >>= \d -> (,) d <$> genRun 4 d) $ \(d0, (d, cs)) -> case perform (Batch cs) (newSession d0) of
        Left e -> counterexample (show e) False
        Right s ->
          fmap (documentSegments . sessionDocument) (undo s) === Right (documentSegments d0)
            .&&. fmap sessionDocument (undo s >>= redo) === Right d
    )
  , ( "a batch with a failing command is rejected"
    , forAll (genDocument >>= \d -> (,) d <$> genRun 4 d) $ \(d0, (_, cs)) ->
        case perform (Batch (cs ++ [RemoveMorph (MorphId maxBound)])) (newSession d0) of
          Left _ -> property True
          Right _ -> counterexample "accepted" False
    )
  , ( "a nested batch round-trips through the codec and gives the same document"
    , forAll (genDocument >>= \d -> (,) d <$> genRun 4 d) $ \(d0, (d, cs)) ->
        let (front, back) = splitAt (length cs `div` 2) cs
            c = Batch [Batch front, Batch back]
         in decodeCommand (encodeCommand c) === Right c .&&. fmap fst (apply c d0) === Right d
    )
  , ( "a journal replays to the same document"
    , forAll (genDocument >>= \d -> (,) d <$> genRun 6 d) $ \(d0, (d, cs)) ->
        fmap sessionDocument (replayJournal d0 (T.unlines (map encodeCommand cs))) === Right d
    )
  , ( "commands keep identifiers unique and below the next free one"
    , forAll (genDocument >>= genRun 6) $ \(d, _) ->
        let ids = concatMap segmentNodeIds (documentSegments d)
         in length ids === length (nub ids) .&&. all (< nextFreeId d) ids
    )
  , ("language text round trip", forAll genLanguage $ \l -> parseLanguage (renderLanguage l) == Right l)
  , ("language binary round trip", forAll genLanguage $ \l -> decodeLanguage (encodeLanguage l) == Right l)
  , ("language text and binary agree", forAll genLanguage $ \l -> fmap renderLanguage (decodeLanguage (encodeLanguage l)) === Right (renderLanguage l))
  , ( "segmentForm returns known pieces that concatenate to the form"
    , forAll ((,) <$> genKnownLanguage <*> textOf "ab" 0 6) $ \(l, f) -> case segmentForm l f of
        Nothing -> property True
        Just pieces -> T.concat pieces === f .&&. all (\p -> sum (filter (> 0) (map snd (candidates l p))) > 0) pieces
    )
  , ( "suggested segmentation applies and keeps the forms of every token"
    , forAll ((,) <$> genKnownLanguage <*> genKnownDocument 1) $ \(l, d) -> case applyAll (suggestSegmentation l d) d of
        Left e -> counterexample (show e) False
        Right d' -> map (T.concat . map morphForm . tokenMorphs) (concatMap segmentTokens (documentSegments d')) === map (T.concat . map morphForm . tokenMorphs) (concatMap segmentTokens (documentSegments d))
    )
  , ( "suggested glosses apply and give each unglossed morph its best candidate"
    , forAll ((,) <$> genKnownLanguage <*> genKnownDocument 3) $ \(l, d) -> case applyAll (suggestGlosses l d) d of
        Left e -> counterexample (show e) False
        Right d' ->
          let expected =
                [ case morphGloss m of
                    Nothing -> suggestAfter l prev (morphForm m)
                    glossed -> glossed
                | t <- concatMap segmentTokens (documentSegments d)
                , (prev, m) <- zip (Nothing : map (Just . morphForm) (tokenMorphs t)) (tokenMorphs t)
                ]
           in map morphGloss (allMorphs d') === expected
    )
  , ( "learning from an unchanged document changes nothing"
    , forAll ((,) <$> genKnownLanguage <*> genKnownDocument 3) $ \(l, d) -> learnChange d d l === l
    )
  , ( "setting a gloss on an unglossed morph confirms its pair once"
    , forAll ((,,) <$> genKnownLanguage <*> genKnownDocument 3 <*> genKnownGloss) $ \(l, d, g) ->
        case [m | m <- allMorphs d, morphGloss m == Nothing] of
          [] -> property Discard
          m : _ -> case apply (SetGloss (morphId m) (Just g)) d of
            Left e -> counterexample (show e) False
            Right (d', _) -> weightOf (learnChange d d' l) (morphForm m) g === weightOf l (morphForm m) g + 1
    )
  , ( "setting a gloss also confirms the pair after the previous morph of the token"
    , forAll ((,,) <$> genKnownLanguage <*> genKnownDocument 3 <*> genKnownGloss) $ \(l, d, g) ->
        case [ (prev, m)
             | t <- concatMap segmentTokens (documentSegments d)
             , (prev, m) <- zip (Nothing : map (Just . morphForm) (tokenMorphs t)) (tokenMorphs t)
             , morphGloss m == Nothing
             ] of
          [] -> property Discard
          (prev, m) : _ -> case apply (SetGloss (morphId m) (Just g)) d of
            Left e -> counterexample (show e) False
            Right (d', _) ->
              let l' = learnChange d d' l
                  gain = maybe 1 (const 2) prev
               in weightAfter l' prev (morphForm m) g === weightAfter l prev (morphForm m) g + gain
    )
  , ( "replacing a gloss rejects the old pair and confirms the new one"
    , forAll ((,,) <$> genKnownLanguage <*> genKnownDocument 3 <*> genKnownGloss) $ \(l, d, g) ->
        case [(m, old) | m <- allMorphs d, Just old <- [morphGloss m], old /= g] of
          [] -> property Discard
          (m, old) : _ -> case apply (SetGloss (morphId m) (Just g)) d of
            Left e -> counterexample (show e) False
            Right (d', _) ->
              let l' = learnChange d d' l
                  form = morphForm m
               in weightOf l' form old === weightOf l form old - 1 .&&. weightOf l' form g === weightOf l form g + 1
    )
  , ( "clearing a gloss rejects its pair"
    , forAll ((,) <$> genKnownLanguage <*> genKnownDocument 3) $ \(l, d) ->
        case [(m, old) | m <- allMorphs d, Just old <- [morphGloss m]] of
          [] -> property Discard
          (m, old) : _ -> case apply (SetGloss (morphId m) Nothing) d of
            Left e -> counterexample (show e) False
            Right (d', _) -> weightOf (learnChange d d' l) (morphForm m) old === weightOf l (morphForm m) old - 1
    )
  , ( "removing a glossed morph does not change the language"
    , forAll ((,) <$> genKnownLanguage <*> genKnownDocument 3) $ \(l, d) ->
        case [m | m <- allMorphs d, morphGloss m /= Nothing] of
          [] -> property Discard
          m : _ -> case apply (RemoveMorph (morphId m)) d of
            Left e -> counterexample (show e) False
            Right (d', _) -> learnChange d d' l === l
    )
  , ( "validation reports exactly the glossed morphs with undeclared tags"
    , forAll ((,) <$> genKnownLanguage <*> genKnownDocument 3) $ \(l, d) ->
        let bad m = case morphGloss m of
              Just g -> filter (`notElem` languageTagList l) (glossTags g)
              Nothing -> []
         in map (\v -> (violationMorph v, violationTags v)) (validateDocument l d) === [(morphId m, bad m) | m <- allMorphs d, not (null (bad m))]
    )
  , ( "conllu output is read back to the same output"
    , forAll genDocument $ \d -> fmap renderConllu (parseConllu (renderConllu d)) === Right (renderConllu d)
    )
  , ( "import keeps the characters of the words"
    , forAll ((,) <$> genLanguage <*> genRawText) $ \(l, t) ->
        T.concat (map tokenSurface (concatMap segmentTokens (documentSegments (importTextWith l t)))) === T.concat (T.words t)
    )
  , ( "import gives unique identifiers and one morph per token"
    , forAll genRawText $ \t ->
        let d = importText t
            ids = concatMap segmentNodeIds (documentSegments d)
         in length ids === length (nub ids) .&&. all ((== 1) . length . tokenMorphs) (concatMap segmentTokens (documentSegments d))
    )
  , ( "moving a token keeps every token"
    , forAll (genDocument `suchThat` (not . null . allTokens)) $ \d ->
        forAll ((,,) <$> elements (allTokens d) <*> elements (map segmentId (documentSegments d)) <*> choose (0, 4)) $ \(t, sid, i) ->
          case apply (MoveToken (tokenId t) sid i) d of
            Left _ -> property Discard
            Right (d', _) -> sort (map tokenId (allTokens d')) === sort (map tokenId (allTokens d))
    )
  , ( "splitting a morph and merging it back restores the document"
    , forAll (genDocument `suchThat` (not . null . allMorphs)) $ \d ->
        forAll ((,) <$> elements (allMorphs d) <*> choose (0, 4)) $ \(m, i) ->
          case apply (SplitMorph (morphId m) i (MorphId (nextFreeId d))) d >>= \(d', _) -> apply (MergeMorph (morphId m)) d' of
            Left _ -> property Discard
            Right (d'', _) -> documentSegments d'' === documentSegments d
    )
  , ( "splitting a token and merging it back restores the document"
    , forAll (genDocument `suchThat` (not . null . allTokens)) $ \d ->
        forAll (elements (allTokens d) >>= \t -> (,,) t <$> choose (0, length (tokenMorphs t)) <*> choose (0, T.length (tokenSurface t))) $ \(t, mi, si) ->
          case apply (SplitToken (tokenId t) mi si (TokenId (nextFreeId d))) d >>= \(d', _) -> apply (MergeToken (tokenId t)) d' of
            Left e -> counterexample (show e) False
            Right (d'', _) -> documentSegments d'' === documentSegments d
    )
  , ( "a truncated binary language is rejected"
    , forAll genLanguage $ \l ->
        let bytes = encodeLanguage l
         in forAll (choose (0, BS.length bytes - 1)) $ \k -> isLeft (decodeLanguage (BS.take k bytes))
    )
  , ( "a binary language with one changed byte is read or rejected"
    , forAll genLanguage $ \l ->
        let bytes = encodeLanguage l
         in forAll ((,) <$> choose (0, BS.length bytes - 1) <*> arbitrary) $ \(k, b) ->
              length (show (decodeLanguage (BS.concat [BS.take k bytes, BS.singleton b, BS.drop (k + 1) bytes]))) >= 0
    )
  , ( "a changed snapshot is read or rejected"
    , forAll (genDocument >>= mutate . renderSnapshot) $ \t -> length (show (parseSnapshot t)) >= 0
    )
  , ( "a changed command line is read or rejected"
    , forAll (genScenario >>= mutate . encodeCommand . snd) $ \t -> length (show (decodeCommand t)) >= 0
    )
  , ( "changed Toolbox text is read or rejected"
    , forAll (genToolboxDocument >>= mutate . renderToolbox) $ \t -> length (show (parseToolbox toolboxLanguage t)) >= 0
    )
  , ( "changed interlinear text is read or rejected"
    , forAll (genLeipzigDocument >>= mutate . renderDocument) $ \t -> length (show (parseLeipzig toolboxLanguage t)) >= 0
    )
  , ( "changed CoNLL-U text is read or rejected"
    , forAll (genDocument >>= mutate . renderConllu) $ \t -> length (show (parseConllu t)) >= 0
    )
  , ( "changed language text is read or rejected"
    , forAll (genLanguage >>= mutate . renderLanguage) $ \t -> length (show (parseLanguage t)) >= 0
    )
  ]
  where
    languageTagList = foldr (:) [] . languageTags
    toolboxLanguage = mkLanguage "t" tagNames []
