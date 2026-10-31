-- SPDX-License-Identifier: GPL-3.0-or-later
module Suz.Html
  ( renderHtml
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Suz.Model

-- | A complete HTML page of a document, with its style sheet inside. Every
-- segment that has tokens is a list item and every token a word with three
-- lines: the surface form, the morph forms and the glosses, the last two joined
-- by hyphens. Tags are written in lowercase and set in small capitals by the
-- style sheet. A morph without a gloss shows as a question mark. A segment takes the
-- direction of its first strong character. The characters @&@, @<@, @>@, @\"@
-- and @'@ are escaped, and a tab or line break becomes a space.
renderHtml :: Document -> Text
renderHtml doc =
  T.unlines $
    [ "<!DOCTYPE html>"
    , "<html>"
    , "<head>"
    , "<meta charset=\"utf-8\">"
    , "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
    , "<title>Interlinear text</title>"
    , "<style>"
    ]
      ++ styleLines
      ++ [ "</style>"
         , "</head>"
         , "<body>"
         , "<ol class=\"suz\">"
         ]
      ++ concatMap segment (filter (not . null . segmentTokens) (documentSegments doc))
      ++ [ "</ol>"
         , "</body>"
         , "</html>"
         ]

segment :: Segment -> [Text]
segment seg = ["<li dir=\"auto\">"] ++ map token (segmentTokens seg) ++ ["</li>"]

token :: Token -> Text
token t =
  "<div class=\"word\"><span class=\"surface\">"
    <> escape (tokenSurface t)
    <> "</span><span class=\"forms\">"
    <> T.intercalate "-" (map form (tokenMorphs t))
    <> "</span><span class=\"glosses\">"
    <> T.intercalate "-" (map glossOf (tokenMorphs t))
    <> "</span></div>"

form :: Morph -> Text
form m = "<span class=\"form\">" <> escape (morphForm m) <> "</span>"

glossOf :: Morph -> Text
glossOf = maybe "<span class=\"gloss unglossed\">?</span>" gloss . morphGloss

gloss :: Gloss -> Text
gloss (Gloss lexical tags) =
  "<span class=\"gloss\">"
    <> T.intercalate "." ([escape lexical | not (T.null lexical)] ++ map tag tags)
    <> "</span>"
  where
    tag t = "<span class=\"tag\">" <> T.toLower (escape t) <> "</span>"

escape :: Text -> Text
escape = T.concatMap replace
  where
    replace c = case c of
      '&' -> "&amp;"
      '<' -> "&lt;"
      '>' -> "&gt;"
      '"' -> "&quot;"
      '\'' -> "&#39;"
      '\t' -> " "
      '\n' -> " "
      '\r' -> " "
      _ -> T.singleton c

styleLines :: [Text]
styleLines =
  [ ":root { color-scheme: light dark; }"
  , "body { font-family: system-ui, sans-serif; line-height: 1.4; margin: 2rem auto; max-width: 60rem; padding: 0 1rem; }"
  , "ol.suz { list-style: none; margin: 0; padding: 0; display: flex; flex-direction: column; gap: 1rem; counter-reset: segment; }"
  , "ol.suz > li { counter-increment: segment; display: flex; flex-wrap: wrap; gap: 0.75rem 1.5rem; padding: 1rem 1.25rem; border: 1px solid rgba(128, 128, 128, 0.35); border-radius: 0.75rem; }"
  , "ol.suz > li::before { content: counter(segment); opacity: 0.5; font-size: 0.85em; min-width: 1.4em; }"
  , ".word { display: flex; flex-direction: column; align-items: center; white-space: nowrap; }"
  , ".surface { font-weight: 700; font-size: 1.1em; margin-bottom: 0.15rem; }"
  , ".forms { font-style: italic; }"
  , ".tag { font-variant-caps: all-small-caps; letter-spacing: 0.03em; }"
  , ".unglossed { opacity: 0.55; font-weight: 700; }"
  ]
