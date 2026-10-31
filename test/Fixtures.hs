-- SPDX-License-Identifier: GPL-3.0-or-later
module Fixtures where

import Control.Monad (foldM)
import qualified Data.Text as T
import Data.Word (Word64)
import Suz

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
spareMorph = Morph (MorphId (nextFreeId baseDoc)) "х" Nothing Nothing

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

segmentationDoc :: Document
segmentationDoc =
  fst
    ( must
        ( apply
            (InsertSegment 0 (buildSegment emptyDocument [("китаплар", ["китаплар"]), ("мин", ["мин"]), ("лар", ["лар"])]))
            emptyDocument
        )
    )

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

spareId :: MorphId
spareId = MorphId (nextFreeId baseDoc)

firstGloss :: Document -> Maybe Gloss
firstGloss doc = morphGloss (concatMap tokenMorphs (concatMap segmentTokens (documentSegments doc)) !! 0)
