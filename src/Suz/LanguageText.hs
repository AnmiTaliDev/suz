-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.LanguageText
  ( parseLanguage
  , renderLanguage
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Suz.Codec.Escape (decodeLexical, encodeLexical, escape, unescape)
import Suz.Language
import Suz.Model
import Text.Read (readMaybe)

data Line
  = LineName Text
  | LineTag Text
  | LineWeight Text Double Gloss
  | LineContext Text Text Double Gloss

-- | Reads a language from its tab-separated text form: one @language@ line,
-- @tag@ lines, @weight@ lines with the morph form, the weight, the lexical
-- part of the gloss and the gloss tags, and @context@ lines, which are
-- @weight@ lines with the previous morph form in front. An empty lexical part is
-- written as @-@ and a literal @-@ as @\\-@. Blank lines and lines starting
-- with @#@ are skipped. The error holds the line number (0 when the problem
-- is not tied to a line) and a short description.
parseLanguage :: Text -> Either (Int, String) Language
parseLanguage text = do
  parsed <- traverse parseLine [(n, l) | (n, l) <- zip [1 ..] (T.lines text), not (skipped l)]
  name <- case [(n, x) | (n, LineName x) <- parsed] of
    [] -> Left (0, "missing language line")
    [(_, x)] -> Right x
    _ : (n, _) : _ -> Left (n, "duplicate language line")
  let tags = [t | (_, LineTag t) <- parsed]
      entries = [(f, g, w) | (_, LineWeight f w g) <- parsed]
      contexts = [(p, f, g, w) | (_, LineContext p f w g) <- parsed]
  pure (withContext contexts (mkLanguage name tags entries))

-- | Text form of a language, readable by 'parseLanguage'. Tags and forms are
-- sorted, candidates of a form go from the heaviest to the lightest. Context
-- lines follow the weight lines, ordered by previous form and then form.
renderLanguage :: Language -> Text
renderLanguage lang =
  T.unlines $
    ["language\t" <> escape (languageName lang)]
      ++ ["tag\t" <> escape t | t <- Set.toList (languageTags lang)]
      ++ [ weightLine form g w
         | form <- Map.keys (languageWeights lang)
         , (g, w) <- candidates lang form
         ]
      ++ [ contextLine prev form g w
         | (prev, form) <- Map.keys (languageContext lang)
         , (g, w) <- contextCandidates lang prev form
         ]

skipped :: Text -> Bool
skipped l = T.null (T.strip l) || "#" `T.isPrefixOf` l

parseLine :: (Int, Text) -> Either (Int, String) (Int, Line)
parseLine (n, raw) = either (Left . (,) n) (Right . (,) n) (decodeLine raw)

decodeLine :: Text -> Either String Line
decodeLine raw = case T.splitOn "\t" raw of
  ["language", name] -> LineName <$> unescape name
  ["tag", tag] -> LineTag <$> unescape tag
  "weight" : form : w : lexical : tags -> do
    f <- unescape form
    weight <- readWeight w
    l <- decodeLexical lexical
    ts <- traverse unescape tags
    pure (LineWeight f weight (Gloss l ts))
  "context" : prev : form : w : lexical : tags -> do
    p <- unescape prev
    f <- unescape form
    weight <- readWeight w
    l <- decodeLexical lexical
    ts <- traverse unescape tags
    pure (LineContext p f weight (Gloss l ts))
  _ -> Left "unrecognized line"

readWeight :: Text -> Either String Double
readWeight t = case readMaybe (T.unpack t) of
  Just w | not (isNaN w || isInfinite w) -> Right w
  _ -> Left ("bad weight: " ++ T.unpack t)

weightLine :: Text -> Gloss -> Double -> Text
weightLine form (Gloss lexical tags) w =
  T.intercalate "\t" (["weight", escape form, T.pack (show w), encodeLexical lexical] ++ map escape tags)

contextLine :: Text -> Text -> Gloss -> Double -> Text
contextLine prev form (Gloss lexical tags) w =
  T.intercalate
    "\t"
    (["context", escape prev, escape form, T.pack (show w), encodeLexical lexical] ++ map escape tags)
