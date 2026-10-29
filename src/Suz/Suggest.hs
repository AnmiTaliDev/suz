-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Suggest
  ( suggestGlosses
  ) where

import Suz.Command
import Suz.Language
import Suz.Model

-- | One 'SetGloss' for every unglossed morph whose form has a candidate in the
-- language. The commands are not applied.
suggestGlosses :: Language -> Document -> [Command]
suggestGlosses lang doc =
  [ SetGloss (morphId m) (Just g)
  | s <- documentSegments doc
  , t <- segmentTokens s
  , m <- tokenMorphs t
  , Nothing <- [morphGloss m]
  , Just g <- [suggest lang (morphForm m)]
  ]
