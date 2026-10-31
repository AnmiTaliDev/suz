-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Latex
  ( renderLatex
  ) where

import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Model

-- | LaTeX source of a document for the @gb4e@ package: one @exe@ environment
-- with an example for every segment that has tokens, each a three-line
-- @\\glll@ of surface forms, morph forms joined with hyphens and glosses joined
-- with hyphens. Tags are set in small capitals in lowercase, as in the Leipzig
-- Glossing Rules. A morph without a gloss shows as a question mark. Special
-- characters are escaped, a tab or line break becomes a space, and a word that
-- is empty or has a space is put in braces. A document without such segments
-- gives the empty text.
renderLatex :: Document -> Text
renderLatex doc = case filter (not . null . segmentTokens) (documentSegments doc) of
  [] -> ""
  segments -> T.unlines (["\\begin{exe}"] ++ concatMap example segments ++ ["\\end{exe}"])

example :: Segment -> [Text]
example seg =
  [ "\\ex"
  , "\\glll " <> row (map tokenSurfaceCell tokens) <> "\\\\"
  , "      " <> row (map tokenMorphsCell tokens) <> "\\\\"
  , "      " <> row (map tokenGlossesCell tokens) <> "\\\\"
  ]
  where
    tokens = segmentTokens seg

row :: [Text] -> Text
row cells = T.unwords (map word cells) <> " "
  where
    word cell
      | T.null cell || T.any isSpace cell = "{" <> cell <> "}"
      | otherwise = cell

tokenSurfaceCell :: Token -> Text
tokenSurfaceCell = escape . tokenSurface

tokenMorphsCell :: Token -> Text
tokenMorphsCell = T.intercalate "-" . map (escape . morphForm) . tokenMorphs

tokenGlossesCell :: Token -> Text
tokenGlossesCell = T.intercalate "-" . map (maybe "?" glossText . morphGloss) . tokenMorphs

glossText :: Gloss -> Text
glossText (Gloss lexical tags) =
  T.intercalate "." ([escape lexical | not (T.null lexical)] ++ [smallCaps | not (null tags)])
  where
    smallCaps = "\\textsc{" <> T.intercalate "." (map (T.toLower . escape) tags) <> "}"

escape :: Text -> Text
escape = T.concatMap replace
  where
    replace c = case c of
      '\\' -> "\\textbackslash{}"
      '&' -> "\\&"
      '%' -> "\\%"
      '$' -> "\\$"
      '#' -> "\\#"
      '_' -> "\\_"
      '{' -> "\\{"
      '}' -> "\\}"
      '~' -> "\\textasciitilde{}"
      '^' -> "\\textasciicircum{}"
      '<' -> "\\textless{}"
      '>' -> "\\textgreater{}"
      '|' -> "\\textbar{}"
      '\t' -> " "
      '\n' -> " "
      '\r' -> " "
      _ -> T.singleton c
