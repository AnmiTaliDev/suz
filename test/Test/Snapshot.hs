-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Snapshot (checks) where

import qualified Data.Text as T
import Fixtures
import Suz

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

checks :: [(String, Bool)]
checks =
  [ ("snapshot round trip", all (\d -> parseSnapshot (renderSnapshot d) == Right d) snapshotDocs)
  , ("snapshot is stable", all (\d -> fmap renderSnapshot (parseSnapshot (renderSnapshot d)) == Right (renderSnapshot d)) snapshotDocs)
  , ("snapshot keeps next id", fmap documentNextId (parseSnapshot (renderSnapshot (after (RemoveSegment (segmentId plainSegment))))) == Right (nextFreeId baseDoc))
  , ("snapshot distinguishes absent and empty gloss", parseSnapshot "document\t3\nsegment\t0\ntoken\t1\tt\nmorph\t2\tm\t-\n" == Right (Document 3 [Segment (SegmentId 0) [Token (TokenId 1) "t" [Morph (MorphId 2) "m" (Just (Gloss "" [])) Nothing]]]))
  , ("snapshot layout", renderSnapshot baseDoc == "suz-snapshot\t1\ndocument\t6\nsegment\t0\ntoken\t1\tкитаплар\nmorph\t2\tкитап\tbook\nmorph\t3\tлар\t-\tPL\ntoken\t4\tмин\nmorph\t5\tмин\n")
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
  , ("snapshot round trip with UD fields", parseSnapshot (renderSnapshot udDoc) == Right udDoc)
  , ("snapshot UD layout", renderSnapshot udDoc == "suz-snapshot\t1\ndocument\t6\nsegment\t0\ntoken\t1\tкитаплар\nmorph\t2\tкитап\tbook\nud\tkitap\tNOUN\t-\tNumber=Plur\t3\tobj\t-\tSpaceAfter=No\nmorph\t3\tлар\t-\tPL\ntoken\t4\tмин\nmorph\t5\tмин\n")
  , ("snapshot UD root and absent head", all (\h -> parseSnapshot (renderSnapshot (udDocWith udExample {udHead = h})) == Right (udDocWith udExample {udHead = h})) [NoHead, RootHead, HeadOf (MorphId 0)])
  , ("snapshot UD escapes special characters", let u = udExample {udFeats = "a\tb\\c", udMisc = "-", udLemma = ""} in parseSnapshot (renderSnapshot (udDocWith u)) == Right (udDocWith u))
  , ("snapshot rejects a UD line without a morph", errorOf (parseSnapshot "document\t2\nsegment\t0\nud\t-\t-\t-\t-\t-\t-\t-\t-\n") == Just (3, "line out of place"))
  , ("snapshot rejects two UD lines for one morph", errorOf (parseSnapshot (T.unlines ["document\t3", "segment\t0", "token\t1\tt", "morph\t2\tm", udLine, udLine])) == Just (6, "line out of place"))
  , ("snapshot rejects a UD line with a wrong field count", errorOf (parseSnapshot "document\t3\nsegment\t0\ntoken\t1\tt\nmorph\t2\tm\nud\ta\n") == Just (5, "expected 8 UD fields"))
  , ("snapshot rejects a bad UD head", errorOf (parseSnapshot "document\t3\nsegment\t0\ntoken\t1\tt\nmorph\t2\tm\nud\t-\t-\t-\t-\tx\t-\t-\t-\n") == Just (5, "bad head: x"))
  ]

udExample :: UdFields
udExample = UdFields "kitap" "NOUN" "" "Number=Plur" (HeadOf mA2) "obj" "" "SpaceAfter=No"

udDocWith :: UdFields -> Document
udDocWith u = baseDoc {documentSegments = [s {segmentTokens = [t {tokenMorphs = [if morphId m == mA1 then m {morphUd = Just u} else m | m <- tokenMorphs t]} | t <- segmentTokens s] } | s <- documentSegments baseDoc]}

udDoc :: Document
udDoc = udDocWith udExample

udLine :: T.Text
udLine = "ud\t-\t-\t-\t-\t-\t-\t-\t-"
