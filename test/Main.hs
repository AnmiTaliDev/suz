-- SPDX-License-Identifier: GPL-3.0-or-later
module Main (main) where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Word (Word64, Word8)
import Suz
import System.Exit (exitFailure, exitSuccess)

must :: Show e => Either e a -> a
must = either (error . show) id

errorOf :: Either e a -> Maybe e
errorOf = either Just (const Nothing)

rawToken :: Token -> Word64
rawToken t = case tokenId t of TokenId n -> n

plainSegment :: Segment
plainSegment = buildSegment emptyDocument [("китаплар", ["китап", "лар"]), ("мин", ["мин"])]

plainDoc :: Document
plainDoc = fst (must (apply (InsertSegment 0 plainSegment) emptyDocument))

tokA, tokB :: Token
tokA = segmentTokens plainSegment !! 0
tokB = segmentTokens plainSegment !! 1

mA1, mA2 :: MorphId
mA1 = morphId (tokenMorphs tokA !! 0)
mA2 = morphId (tokenMorphs tokA !! 1)

glossBook, glossPl :: Gloss
glossBook = Gloss "book" []
glossPl = Gloss "" ["PL"]

baseDoc :: Document
baseDoc =
  fst (must (apply (Batch [SetGloss mA1 (Just glossBook), SetGloss mA2 (Just glossPl)]) plainDoc))

spareMorph :: Morph
spareMorph = Morph (MorphId (nextFreeId baseDoc)) "х" Nothing

commands :: [Command]
commands =
  [ InsertSegment 1 (buildSegment baseDoc [("a", ["a", "b"])])
  , RemoveSegment (segmentId plainSegment)
  , InsertToken (segmentId plainSegment) 1 (buildToken baseDoc "ва" ["ва"])
  , RemoveToken (tokenId tokA)
  , SetSurface (tokenId tokA) "Китаплар"
  , InsertMorph (tokenId tokA) 2 spareMorph
  , RemoveMorph mA1
  , SetForm mA1 "кит"
  , SetGloss mA1 (Just (Gloss "x" ["A", "B"]))
  , SetGloss mA1 Nothing
  , SplitMorph mA1 2 (morphId spareMorph)
  , MergeMorph mA1
  , Batch [SetForm mA1 "x", SetGloss mA1 Nothing, RemoveMorph mA2]
  , SetSurface (tokenId tokB) "a\tb\nc\\d\r"
  , SplitToken (tokenId tokA) 1 5 splitTokenId
  , SplitToken (tokenId tokA) 0 0 splitTokenId
  , SplitToken (tokenId tokA) 2 8 splitTokenId
  , MergeToken (tokenId tokA)
  , MoveToken (tokenId tokB) (segmentId plainSegment) 0
  , MoveToken (tokenId tokA) (segmentId plainSegment) 1
  ]

roundTripsIn :: Document -> Command -> Bool
roundTripsIn doc c = case apply c doc >>= \(d, inverse) -> apply inverse d of
  Right (d', _) -> documentSegments d' == documentSegments doc
  Left _ -> False

roundTrips :: Command -> Bool
roundTrips = roundTripsIn baseDoc

tokenSurfaces :: Document -> [[String]]
tokenSurfaces doc = [[T.unpack (tokenSurface t) | t <- segmentTokens s] | s <- documentSegments doc]

splitTokenId :: TokenId
splitTokenId = TokenId (nextFreeId baseDoc)

otherSegment :: Segment
otherSegment = buildSegment baseDoc [("z", ["z"])]

twoSegmentDoc :: Document
twoSegmentDoc = fst (must (apply (InsertSegment 1 otherSegment) baseDoc))

forms :: Document -> [String]
forms doc =
  [ T.unpack (morphForm m)
  | s <- documentSegments doc
  , t <- segmentTokens s
  , m <- tokenMorphs t
  ]

badTagDoc :: Document
badTagDoc = after (SetGloss mA1 (Just (Gloss "x" ["ACC", "PL", "FOO"])))

twoBadTagsDoc :: Document
twoBadTagsDoc =
  fst (must (apply (SetGloss mA2 (Just (Gloss "" ["BAR"]))) badTagDoc))

segLanguage :: Language
segLanguage =
  mkLanguage
    "seg"
    ["PL", "ACC"]
    [ ("китап", glossBook, 3)
    , ("лар", glossPl, 3)
    , ("ны", Gloss "" ["ACC"], 1)
    , ("ab", glossPl, 1)
    , ("abab", glossPl, 1)
    , ("a", glossPl, 1)
    , ("b", glossPl, 1)
    , ("c", glossPl, 1)
    , ("bc", glossPl, 6)
    , ("neg", glossPl, -1)
    ]

segmentationDoc :: Document
segmentationDoc =
  fst
    ( must
        ( apply
            (InsertSegment 0 (buildSegment emptyDocument [("китаплар", ["китаплар"]), ("мин", ["мин"]), ("лар", ["лар"])]))
            emptyDocument
        )
    )

appliedSegmentation :: Language -> Document -> Document
appliedSegmentation lang doc = fst (must (apply (Batch (suggestSegmentation lang doc)) doc))

snapshotDocs :: [Document]
snapshotDocs =
  [ emptyDocument
  , plainDoc
  , baseDoc
  , twoSegmentDoc
  , segmentationDoc
  , sessionDocument session2
  , after (RemoveSegment (segmentId plainSegment))
  , after (SetSurface (tokenId tokA) "a\tb\nc\\d\r")
  , after (SetGloss mA1 (Just (Gloss "-" ["x\ty"])))
  ]

learnSegment :: Segment
learnSegment = buildSegment emptyDocument [("cats", ["cat", "s"])]

learnDoc :: Document
learnDoc = fst (must (apply (InsertSegment 0 learnSegment) emptyDocument))

mCat, mS :: MorphId
mCat = morphId (tokenMorphs (head (segmentTokens learnSegment)) !! 0)
mS = morphId (tokenMorphs (head (segmentTokens learnSegment)) !! 1)

learnBase :: Language
learnBase = mkLanguage "l" ["PL", "3SG"] []

learnSession :: Session
learnSession = newSession learnDoc

learned :: Language -> [Command] -> Language
learned lang = snd . foldl step (learnSession, lang)
  where
    step (session, l) c = must (performLearning c l session)

after :: Command -> Document
after c = fst (must (apply c baseDoc))

session2 :: Session
session2 =
  must $
    foldM
      (flip perform)
      (newSession emptyDocument)
      [InsertSegment 0 plainSegment, SetForm mA1 "ки\tт\\ап\nx"]

tinyLanguage :: Language
tinyLanguage =
  mkLanguage
    "tiny"
    ["PL", "3SG"]
    [ ("s", glossPl, 5)
    , ("s", Gloss "" ["3SG"], 4)
    , ("s", glossPl, 0.5)
    , ("китап", glossBook, 3)
    , ("лар", glossPl, 3)
    ]

trickyLanguage :: Language
trickyLanguage =
  withContext
    [ ("p\tq", "f\to", Gloss "-" ["A"], 2)
    , ("p\tq", "f\to", Gloss "" [], -0.5)
    , ("z", "x", Gloss "a\nb" [], 1)
    ]
    ( mkLanguage
        "na\tme\\"
        ["A", "B\tC"]
        [ ("f\to", Gloss "-" ["A"], 1.5)
        , ("f\to", Gloss "" ["A", "B\tC"], 0.25)
        , ("x", Gloss "a\nb" [], 2)
        , ("neg", Gloss "n" [], -1.5)
        ]
    )

glossSg3 :: Gloss
glossSg3 = Gloss "" ["3SG"]

contextLanguage :: Language
contextLanguage = withContext [("cat", "s", glossPl, 3), ("walk", "s", glossSg3, 2.5), ("cat", "zz", glossPl, 1)] tinyLanguage

contextDoc :: Document
contextDoc =
  fst
    ( must
        ( apply
            (InsertSegment 0 (buildSegment emptyDocument [("cats", ["cat", "s"]), ("walks", ["walk", "s"]), ("s", ["s"])]))
            emptyDocument
        )
    )

contextGlossLanguage :: Language
contextGlossLanguage =
  withContext
    [("walk", "s", glossSg3, 2)]
    (mkLanguage "ctx" ["PL", "3SG"] [("cat", glossBook, 1), ("walk", Gloss "walk" [], 1), ("s", glossPl, 5.5), ("s", glossSg3, 5)])

v1Header :: [Word8]
v1Header = [0x53, 0x55, 0x5A, 0x4C, 1, 0]

checks :: [(String, Bool)]
checks =
  [ ("every command is undone by its inverse", all roundTrips commands)
  , ("split forms", forms (after (SplitMorph mA1 2 spareId)) == ["ки", "тап", "лар", "мин"])
  , ("merge forms", forms (after (MergeMorph mA1)) == ["китаплар", "мин"])
  , ("merge keeps first gloss", firstGloss (after (MergeMorph mA1)) == Just glossBook)
  , ("remove then insert restores position", roundTrips (RemoveToken (tokenId tokA)))
  , ("unknown token", errorOf (apply (RemoveToken (TokenId 999)) baseDoc) == Just (NoSuchToken (TokenId 999)))
  , ( "duplicate id"
    , errorOf (apply (InsertToken (segmentId plainSegment) 0 tokA) baseDoc)
        == Just (DuplicateId (rawToken tokA))
    )
  , ( "index out of range"
    , errorOf (apply (InsertToken (segmentId plainSegment) 5 (buildToken baseDoc "q" [])) baseDoc)
        == Just (IndexOutOfRange 5)
    )
  , ("split at zero", errorOf (apply (SplitMorph mA1 0 spareId) baseDoc) == Just (BadSplitPoint mA1 0))
  , ("split at end", errorOf (apply (SplitMorph mA1 5 spareId) baseDoc) == Just (BadSplitPoint mA1 5))
  , ("merge last morph", errorOf (apply (MergeMorph mA2) baseDoc) == Just (NoNextMorph mA2))
  , ( "failed batch changes nothing"
    , errorOf (apply (Batch [SetForm mA1 "z", RemoveMorph (MorphId 999)]) baseDoc) == Just (NoSuchMorph (MorphId 999))
    )
  , ("ids are not reused after removal", nextFreeId (after (RemoveSegment (segmentId plainSegment))) == nextFreeId baseDoc)
  , ("codec round trip", all (\c -> decodeCommand (encodeCommand c) == Right c) commands)
  , ("codec rejects unknown command", errorOf (decodeCommand "bogus") /= Nothing)
  , ("codec rejects trailing fields", errorOf (decodeCommand "remove-token\t1\t2") /= Nothing)
  , ("codec rejects bad escape", errorOf (decodeCommand "set-form\t1\ta\\q") /= Nothing)
  , ("undo twice reaches the empty document", fmap (documentSegments . sessionDocument) (undo session2 >>= undo) == Right [])
  , ("undo on a fresh session", errorOf (undo (newSession emptyDocument)) == Just NothingToUndo)
  , ("redo restores the document", fmap sessionDocument (undo session2 >>= redo) == Right (sessionDocument session2))
  , ("perform clears redo", errorOf (undo session2 >>= performAny (SetForm mA1 "q") >>= redo) == Just NothingToRedo)
  , ("journal replay", fmap sessionDocument replayed == Right (sessionDocument session2))
  , ("journal replay keeps history", fmap history replayed == Right (history session2))
  , ("replay parse error", replayParseLine1)
  , ("replay edit error", replayJournal emptyDocument "\nremove-token\t9\n" == Left (ReplayEdit 2 (NoSuchToken (TokenId 9))))
  , ("candidates order", candidates tinyLanguage "s" == [(glossPl, 5.5), (Gloss "" ["3SG"], 4)])
  , ("unknown form", null (candidates tinyLanguage "zzz") && suggest tinyLanguage "zzz" == Nothing)
  , ("confirm changes ranking", suggest (confirmTwice tinyLanguage) "s" == Just (Gloss "" ["3SG"]))
  , ("reject lowers the weight", candidates (reject "s" glossPl tinyLanguage) "s" == [(glossPl, 4.5), (Gloss "" ["3SG"], 4)])
  , ("reject changes ranking", suggest (reject "s" glossPl (reject "s" glossPl tinyLanguage)) "s" == Just (Gloss "" ["3SG"]))
  , ("reject creates a negative entry", candidates (reject "q" glossPl tinyLanguage) "q" == [(glossPl, -1)])
  , ("zero weight is not suggested", suggest (reject "q" glossPl (confirm "q" glossPl (mkLanguage "z" [] []))) "q" == Nothing)
  , ("zero weight stays a candidate", candidates (reject "q" glossPl (confirm "q" glossPl (mkLanguage "z" [] []))) "q" == [(glossPl, 0)])
  , ("confirm undoes reject", candidates (confirm "s" glossPl (reject "s" glossPl tinyLanguage)) "s" == candidates tinyLanguage "s")
  , ("negative weight is not suggested", suggest (reject "q" glossPl (reject "q" glossPl (confirm "q" glossPl (mkLanguage "z" [] [])))) "q" == Nothing)
  , ("valid gloss", isValidGloss tinyLanguage glossPl && not (isValidGloss tinyLanguage (Gloss "" ["ACC"])))
  , ("suggest fills unglossed morphs", suggestions == 2)
  , ("suggest skips glossed morphs", null (suggestGlosses tinyLanguage baseDoc))
  , ("suggested glosses apply", glossesAfterSuggest == [Just glossBook, Just glossPl, Nothing])
  , ("render", renderDocument baseDoc == "китаплар  мин\nкитап-лар мин\nbook-PL   ?")
  , ("split token surfaces", tokenSurfaces (after (SplitToken (tokenId tokA) 1 5 splitTokenId)) == [["китап", "лар", "мин"]])
  , ("split token keeps morph order", forms (after (SplitToken (tokenId tokA) 1 5 splitTokenId)) == ["китап", "лар", "мин"])
  , ("split token moves morphs", map (length . tokenMorphs) (segmentTokens (head (documentSegments (after (SplitToken (tokenId tokA) 1 5 splitTokenId))))) == [1, 1, 1])
  , ("split token at the ends", all (roundTrips . (\(k, j) -> SplitToken (tokenId tokA) k j splitTokenId)) [(0, 0), (2, 8), (0, 8), (2, 0)])
  , ("merge token surfaces", tokenSurfaces (after (MergeToken (tokenId tokA))) == [["китаплармин"]])
  , ("merge token keeps first id", fmap (\d -> map tokenId (segmentTokens (head (documentSegments d)))) (fmap fst (apply (MergeToken (tokenId tokA)) baseDoc)) == Right [tokenId tokA])
  , ("move token within segment", tokenSurfaces (after (MoveToken (tokenId tokB) (segmentId plainSegment) 0)) == [["мин", "китаплар"]])
  , ("move token to another segment", tokenSurfaces (fst (must (apply (MoveToken (tokenId tokA) (segmentId otherSegment) 1) twoSegmentDoc))) == [["мин"], ["z", "китаплар"]])
  , ("move token across segments is undone", roundTripsIn twoSegmentDoc (MoveToken (tokenId tokA) (segmentId otherSegment) 0))
  , ("split token bad morph index", errorOf (apply (SplitToken (tokenId tokA) 3 0 splitTokenId) baseDoc) == Just (BadTokenSplit (tokenId tokA) 3 0))
  , ("split token bad surface index", errorOf (apply (SplitToken (tokenId tokA) 0 9 splitTokenId) baseDoc) == Just (BadTokenSplit (tokenId tokA) 0 9))
  , ("split token negative index", errorOf (apply (SplitToken (tokenId tokA) (-1) 0 splitTokenId) baseDoc) == Just (BadTokenSplit (tokenId tokA) (-1) 0))
  , ("split token duplicate id", errorOf (apply (SplitToken (tokenId tokA) 1 1 (tokenId tokB)) baseDoc) == Just (DuplicateId (rawToken tokB)))
  , ("merge last token", errorOf (apply (MergeToken (tokenId tokB)) baseDoc) == Just (NoNextToken (tokenId tokB)))
  , ("move token to unknown segment", errorOf (apply (MoveToken (tokenId tokA) (SegmentId 999) 0) baseDoc) == Just (NoSuchSegment (SegmentId 999)))
  , ("move token out of range", errorOf (apply (MoveToken (tokenId tokA) (segmentId plainSegment) 5) baseDoc) == Just (IndexOutOfRange 5))
  , ("validate accepts declared tags", null (validateDocument tinyLanguage baseDoc))
  , ("validate skips unglossed morphs", null (validateDocument tinyLanguage plainDoc))
  , ("validate reports unknown tags", validateDocument tinyLanguage badTagDoc == [Violation mA1 ["ACC", "FOO"]])
  , ("validate keeps document order", map violationMorph (validateDocument tinyLanguage twoBadTagsDoc) == [mA1, mA2])
  , ("validate with empty language", map violationMorph (validateDocument (mkLanguage "e" [] []) baseDoc) == [mA2])
  , ("segment known pair", segmentForm segLanguage "китаплар" == Just ["китап", "лар"])
  , ("segment three pieces", segmentForm segLanguage "китапларны" == Just ["китап", "лар", "ны"])
  , ("segment whole known form", segmentForm segLanguage "китап" == Just ["китап"])
  , ("segment unknown remainder", segmentForm segLanguage "китапзз" == Nothing)
  , ("segment unknown form", segmentForm segLanguage "мин" == Nothing)
  , ("segment empty form", segmentForm segLanguage "" == Nothing)
  , ("segment prefers fewer pieces", segmentForm segLanguage "abab" == Just ["abab"] && segmentForm segLanguage "ab" == Just ["ab"])
  , ("segment prefers heavier on equal count", segmentForm segLanguage "abc" == Just ["a", "bc"])
  , ("segment ignores non-positive forms", segmentForm segLanguage "neg" == Nothing)
  , ("suggest segmentation splits single morphs", length (suggestSegmentation segLanguage segmentationDoc) == 1)
  , ("suggested segmentation forms", forms (appliedSegmentation segLanguage segmentationDoc) == ["китап", "лар", "мин", "лар"])
  , ("suggested segmentation is undone", roundTripsIn segmentationDoc (Batch (suggestSegmentation segLanguage segmentationDoc)))
  , ("suggested segmentation ids are fresh", all (\c -> case c of SplitMorph _ _ (MorphId n) -> n >= nextFreeId segmentationDoc; _ -> False) (suggestSegmentation segLanguage segmentationDoc))
  , ("suggest segmentation with three pieces", length (suggestSegmentation segLanguage (appliedWith "китапларны")) == 2)
  , ("suggest segmentation skips segmented tokens", null (suggestSegmentation segLanguage baseDoc))
  , ("suggest segmentation skips glossed morphs", null (suggestSegmentation segLanguage (after (SetGloss mA1 (Just glossBook)))))
  , ("context raw candidates", contextCandidates contextLanguage "walk" "s" == [(glossSg3, 2.5)])
  , ("context adds to plain weights", candidatesAfter contextLanguage (Just "walk") "s" == [(glossSg3, 6.5), (glossPl, 5.5)])
  , ("context after cat", candidatesAfter contextLanguage (Just "cat") "s" == [(glossPl, 8.5), (glossSg3, 4)])
  , ("no previous form equals plain candidates", candidatesAfter contextLanguage Nothing "s" == candidates contextLanguage "s")
  , ("unknown previous form equals plain candidates", candidatesAfter contextLanguage (Just "dog") "s" == candidates contextLanguage "s")
  , ("context-only candidates", candidatesAfter contextLanguage (Just "cat") "zz" == [(glossPl, 1)])
  , ("suggest after", suggestAfter contextLanguage (Just "walk") "s" == Just glossSg3 && suggest contextLanguage "s" == Just glossPl)
  , ("confirm after flips ranking", suggestAfter (confirmAfter "walk" "s" glossSg3 (confirmAfter "walk" "s" glossSg3 tinyLanguage)) (Just "walk") "s" == Just glossSg3)
  , ("confirm after leaves plain weights", candidates (confirmAfter "walk" "s" glossSg3 tinyLanguage) "s" == candidates tinyLanguage "s")
  , ("reject after lowers context weight", contextCandidates (rejectAfter "walk" "s" glossSg3 tinyLanguage) "walk" "s" == [(glossSg3, -1)])
  , ("reject after cancels confirm after", contextCandidates (rejectAfter "p" "q" glossPl (confirmAfter "p" "q" glossPl tinyLanguage)) "p" "q" == [(glossPl, 0)])
  , ("with context sums repeated entries", contextCandidates (withContext [("p", "q", glossPl, 1)] (withContext [("p", "q", glossPl, 2)] tinyLanguage)) "p" "q" == [(glossPl, 3)])
  , ( "suggested glosses use context"
    , [g | Just g <- map (\c -> case c of SetGloss _ g' -> g'; _ -> Nothing) (suggestGlosses contextGlossLanguage contextDoc)]
        == [glossBook, glossPl, Gloss "walk" [], glossSg3, glossPl]
    )
  , ("language text round trip with context", parseLanguage (renderLanguage contextLanguage) == Right contextLanguage)
  , ("language text context line", parseLanguage "language\ta\ncontext\tp\tx\t2\t-\tT\n" == Right (withContext [("p", "x", Gloss "" ["T"], 2)] (mkLanguage "a" [] [])))
  , ("language text rejects short context line", errorOf (parseLanguage "language\ta\ncontext\tp\tx\t1\n") == Just (2, "unrecognized line"))
  , ("language binary round trip with context", decodeLanguage (encodeLanguage contextLanguage) == Right contextLanguage)
  , ( "language binary context layout"
    , encodeLanguage (withContext [("p", "x", Gloss "" [], 1)] (mkLanguage "a" [] []))
        == BS.pack (header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0] ++ [1, 0, 0, 0, 0x70] ++ [1, 0, 0, 0, 0x78] ++ [1, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0])
    )
  , ( "language binary reads version 1"
    , decodeLanguage (BS.pack (v1Header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0] ++ [1, 0, 0, 0, 0x78] ++ [1, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0]))
        == Right (mkLanguage "a" [] [("x", Gloss "" [], 1)])
    )
  , ("language binary version 1 has no context section", errorOf (decodeLanguage (BS.pack (v1Header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0]))) == Just "trailing data")
  , ("snapshot round trip", all (\d -> parseSnapshot (renderSnapshot d) == Right d) snapshotDocs)
  , ("snapshot is stable", all (\d -> fmap renderSnapshot (parseSnapshot (renderSnapshot d)) == Right (renderSnapshot d)) snapshotDocs)
  , ("snapshot keeps next id", fmap documentNextId (parseSnapshot (renderSnapshot (after (RemoveSegment (segmentId plainSegment))))) == Right (nextFreeId baseDoc))
  , ("snapshot distinguishes absent and empty gloss", parseSnapshot "document\t3\nsegment\t0\ntoken\t1\tt\nmorph\t2\tm\t-\n" == Right (Document 3 [Segment (SegmentId 0) [Token (TokenId 1) "t" [Morph (MorphId 2) "m" (Just (Gloss "" []))]]]))
  , ("snapshot layout", renderSnapshot baseDoc == "document\t6\nsegment\t0\ntoken\t1\tкитаплар\nmorph\t2\tкитап\tbook\nmorph\t3\tлар\t-\tPL\ntoken\t4\tмин\nmorph\t5\tмин\n")
  , ("snapshot skips comments and blanks", parseSnapshot "# c\n\ndocument\t0\n   \n" == Right emptyDocument)
  , ("snapshot allows continuing the edit", either (const False) (\d -> either (const False) (const True) (apply (InsertSegment 0 (buildSegment d [("a", ["a"])])) d)) (parseSnapshot (renderSnapshot baseDoc)))
  , ("snapshot of restored document continues the session", either (const False) (\d -> either (const False) (const True) (perform (InsertSegment 0 (buildSegment d [("a", ["a"])])) (newSession d))) (parseSnapshot (renderSnapshot baseDoc)))
  , ("snapshot missing document line", errorOf (parseSnapshot "segment\t0\n") == Just (0, "missing document line"))
  , ("snapshot empty input", errorOf (parseSnapshot "") == Just (0, "missing document line"))
  , ("snapshot second document line", errorOf (parseSnapshot "document\t1\ndocument\t1\n") == Just (2, "line out of place"))
  , ("snapshot token outside segment", errorOf (parseSnapshot "document\t2\ntoken\t0\tt\n") == Just (2, "line out of place"))
  , ("snapshot morph outside token", errorOf (parseSnapshot "document\t2\nsegment\t0\nmorph\t1\tm\n") == Just (3, "line out of place"))
  , ("snapshot duplicate id", errorOf (parseSnapshot "document\t3\nsegment\t0\ntoken\t1\tt\ntoken\t1\tu\n") == Just (4, "duplicate identifier 1"))
  , ("snapshot id not below next", errorOf (parseSnapshot "document\t1\nsegment\t1\n") == Just (2, "identifier 1 is not below the next free identifier"))
  , ("snapshot bad number", errorOf (parseSnapshot "document\t-1\n") == Just (1, "bad number: -1"))
  , ("snapshot unrecognized line", errorOf (parseSnapshot "document\t1\nbogus\n") == Just (2, "unrecognized line"))
  , ("snapshot bad escape", errorOf (parseSnapshot "document\t2\nsegment\t0\ntoken\t1\ta\\q\n") == Just (3, "unknown escape"))
  , ( "learn confirms a new gloss with context"
    , let l = learned learnBase [SetGloss mS (Just glossPl)]
       in candidates l "s" == [(glossPl, 1)] && contextCandidates l "cat" "s" == [(glossPl, 1)]
    )
  , ( "learn has no context for the first morph"
    , let l = learned learnBase [SetGloss mCat (Just glossBook)]
       in candidates l "cat" == [(glossBook, 1)] && null (languageContext l)
    )
  , ( "learn replaces a gloss"
    , let l = learned learnBase [SetGloss mS (Just glossPl), SetGloss mS (Just glossSg3)]
       in candidates l "s" == [(glossSg3, 1), (glossPl, 0)] && contextCandidates l "cat" "s" == [(glossSg3, 1), (glossPl, 0)]
    )
  , ( "learn ignores an unchanged gloss"
    , learned learnBase [SetGloss mS (Just glossPl), SetGloss mS (Just glossPl)] == learned learnBase [SetGloss mS (Just glossPl)]
    )
  , ( "learn rejects a cleared gloss"
    , let l = learned learnBase [SetGloss mS (Just glossPl), SetGloss mS Nothing]
       in candidates l "s" == [(glossPl, 0)] && contextCandidates l "cat" "s" == [(glossPl, 0)]
    )
  , ( "learn from a batch"
    , let l = learned learnBase [Batch [SetGloss mCat (Just glossBook), SetGloss mS (Just glossPl)]]
       in candidates l "cat" == [(glossBook, 1)] && candidates l "s" == [(glossPl, 1)]
    )
  , ( "learn rejects with the old form"
    , let l = learned learnBase [SetGloss mS (Just glossPl), Batch [SetForm mS "es", SetGloss mS (Just glossSg3)]]
       in candidates l "s" == [(glossPl, 0)] && candidates l "es" == [(glossSg3, 1)] && contextCandidates l "cat" "es" == [(glossSg3, 1)]
    )
  , ( "learn from an inserted glossed morph"
    , let l = learned learnBase [InsertMorph (tokenId (head (segmentTokens learnSegment))) 2 (Morph (MorphId (nextFreeId learnDoc)) "x" (Just glossPl))]
       in candidates l "x" == [(glossPl, 1)] && contextCandidates l "s" "x" == [(glossPl, 1)]
    )
  , ( "learn ignores commands without glosses"
    , learned learnBase [SetForm mS "es", SetSurface (tokenId (head (segmentTokens learnSegment))) "q"] == learnBase
    )
  , ("learn from identical documents", learnChange baseDoc baseDoc tinyLanguage == tinyLanguage)
  , ( "failed command teaches nothing"
    , fmap (const ()) (performLearning (SetGloss (MorphId 999) (Just glossPl)) learnBase learnSession) == Left (NoSuchMorph (MorphId 999))
    )
  , ( "learning loop fixes the suggestion"
    , let lang = mkLanguage "l" ["PL", "3SG"] [("cat", glossBook, 1), ("s", glossPl, 1), ("s", glossSg3, 0.5)]
          suggested = must (perform (Batch (suggestGlosses lang learnDoc)) learnSession)
          (fixed, lang') = must (performLearning (SetGloss mS (Just glossSg3)) lang suggested)
       in suggestAfter lang (Just "cat") "s" == Just glossPl
            && suggestAfter lang' (Just "cat") "s" == Just glossSg3
            && fmap morphGloss (lookupMorph mS (sessionDocument fixed)) == Just (Just glossSg3)
    )
  , ("language text round trip", parseLanguage (renderLanguage tinyLanguage) == Right tinyLanguage)
  , ("language text round trip with escapes", parseLanguage (renderLanguage trickyLanguage) == Right trickyLanguage)
  , ("language text is stable", fmap renderLanguage (parseLanguage (renderLanguage trickyLanguage)) == Right (renderLanguage trickyLanguage))
  , ("language text placeholders", parseLanguage "language\ta\nweight\tx\t1\t-\tT\nweight\ty\t1\t\\-\n" == Right (mkLanguage "a" [] [("x", Gloss "" ["T"], 1), ("y", Gloss "-" [], 1)]))
  , ("language text skips comments and blanks", parseLanguage "# c\n\nlanguage\ta\n   \n# d\n" == Right (mkLanguage "a" [] []))
  , ("language text sums repeated entries", fmap (`candidates` "x") (parseLanguage "language\ta\nweight\tx\t1\t-\tT\nweight\tx\t2\t-\tT\n") == Right [(Gloss "" ["T"], 3)])
  , ("language text missing name", errorOf (parseLanguage "tag\tPL\n") == Just (0, "missing language line"))
  , ("language text duplicate name", errorOf (parseLanguage "language\ta\nlanguage\tb\n") == Just (2, "duplicate language line"))
  , ("language text rejects NaN", errorOf (parseLanguage "language\ta\nweight\tx\tNaN\t-\n") == Just (2, "bad weight: NaN"))
  , ("language text rejects bad number", errorOf (parseLanguage "language\ta\nweight\tx\tone\t-\n") == Just (2, "bad weight: one"))
  , ("language text rejects unknown line", errorOf (parseLanguage "language\ta\nbogus\n") == Just (2, "unrecognized line"))
  , ("language text rejects short weight line", errorOf (parseLanguage "language\ta\nweight\tx\t1\n") == Just (2, "unrecognized line"))
  , ("language binary round trip", decodeLanguage (encodeLanguage tinyLanguage) == Right tinyLanguage)
  , ("language binary round trip with escapes", decodeLanguage (encodeLanguage trickyLanguage) == Right trickyLanguage)
  , ("language binary is deterministic", fmap encodeLanguage (decodeLanguage trickyBytes) == Right trickyBytes)
  , ("language binary agrees with text", fmap renderLanguage (decodeLanguage trickyBytes) == Right (renderLanguage trickyLanguage))
  , ("language binary empty layout", encodeLanguage (mkLanguage "a" [] []) == BS.pack (header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]))
  , ( "language binary weight layout"
    , encodeLanguage (mkLanguage "a" [] [("x", Gloss "" [], 1)])
        == BS.pack (header ++ [1, 0, 0, 0, 0x61] ++ [0, 0, 0, 0] ++ [1, 0, 0, 0] ++ [1, 0, 0, 0, 0x78] ++ [1, 0, 0, 0] ++ [0, 0, 0, 0, 0, 0, 0xF0, 0x3F] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0] ++ [0, 0, 0, 0])
    )
  , ("language binary rejects bad magic", errorOf (decodeLanguage (BS.pack (0x58 : drop 1 header ++ [1, 0, 0, 0, 0x61, 0, 0, 0, 0, 0, 0, 0, 0]))) == Just "bad magic")
  , ("language binary rejects other version", errorOf (decodeLanguage (BS.pack (take 4 header ++ [3, 0] ++ [1, 0, 0, 0, 0x61, 0, 0, 0, 0, 0, 0, 0, 0]))) == Just "unsupported version: 3")
  , ("language binary rejects truncated data", all (\n -> errorOf (decodeLanguage (BS.take n trickyBytes)) /= Nothing) [0 .. BS.length trickyBytes - 1])
  , ("language binary rejects trailing data", errorOf (decodeLanguage (trickyBytes <> BS.pack [0])) == Just "trailing data")
  , ("language binary rejects invalid UTF-8", errorOf (decodeLanguage (BS.pack (header ++ [1, 0, 0, 0, 0xFF, 0, 0, 0, 0, 0, 0, 0, 0]))) /= Nothing)
  , ("language binary rejects oversized length", errorOf (decodeLanguage (BS.pack (header ++ [0xFF, 0xFF, 0xFF, 0xFF]))) == Just "unexpected end of data")
  , ("language binary rejects non-finite weight", errorOf (decodeLanguage (encodeLanguage (mkLanguage "a" [] [("x", Gloss "" [], 0 / 0)]))) == Just "non-finite weight")
  ]
  where
    lookupMorph mid doc = fmap (\(_, _, m) -> m) (findMorph mid doc)
    appliedWith surface =
      fst (must (apply (InsertSegment 0 (buildSegment emptyDocument [(surface, [surface])])) emptyDocument))
    header = [0x53, 0x55, 0x5A, 0x4C, 2, 0]
    trickyBytes = encodeLanguage trickyLanguage
    spareId = MorphId (nextFreeId baseDoc)
    firstGloss doc = morphGloss (head (concatMap tokenMorphs (concatMap segmentTokens (documentSegments doc))))
    performAny c = either (Left . JournalEdit) Right . perform c
    replayed = replayJournal emptyDocument (renderJournal session2)
    replayParseLine1 = case replayJournal emptyDocument "bogus\n" of
      Left (ReplayParse 1 _) -> True
      _ -> False
    confirmTwice = confirm "s" (Gloss "" ["3SG"]) . confirm "s" (Gloss "" ["3SG"])
    suggestions = length (suggestGlosses tinyLanguage plainDoc)
    glossesAfterSuggest =
      let doc = fst (must (apply (Batch (suggestGlosses tinyLanguage plainDoc)) plainDoc))
       in [morphGloss m | s <- documentSegments doc, t <- segmentTokens s, m <- tokenMorphs t]

main :: IO ()
main = do
  let failed = [name | (name, False) <- checks]
  mapM_ (putStrLn . ("FAIL: " ++)) failed
  putStrLn (show (length checks - length failed) ++ "/" ++ show (length checks) ++ " passed")
  if null failed then exitSuccess else exitFailure
