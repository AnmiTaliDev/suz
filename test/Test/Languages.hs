-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Languages (checks) where

import Control.Monad (foldM)
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Fixtures
import Suz

checks :: IO [(String, Bool)]
checks = concat <$> mapM fileChecks files

fileChecks :: (String, [(Text, Text)]) -> IO [(String, Bool)]
fileChecks (name, examples) = do
  bytes <- BS.readFile ("languages/" ++ name ++ ".language")
  pure $ case parseLanguage (TE.decodeUtf8 bytes) of
    Left _ -> [(name ++ " language file is read", False)]
    Right lang ->
      (name ++ " language file is read", True)
        : concat
          [ [ (name ++ " example " ++ show i ++ " is annotated", annotated lang phrase == expected)
            , (name ++ " example " ++ show i ++ " passes validation", null (validateDocument lang (annotatedDocument lang phrase)))
            ]
          | (i, (phrase, expected)) <- zip [1 :: Int ..] examples
          ]

annotatedDocument :: Language -> Text -> Document
annotatedDocument lang text = sessionDocument glossed
  where
    imported = newSession (importTextWith lang text)
    segmented = must (foldM (flip perform) imported (suggestSegmentation lang (sessionDocument imported)))
    glossed = must (foldM (flip perform) segmented (suggestGlosses lang (sessionDocument segmented)))

annotated :: Language -> Text -> Text
annotated lang text = renderDocument (annotatedDocument lang text)

files :: [(String, [(Text, Text)])]
files =
  [ ("ancient-hebrew",
      [ ("בְּרֵאשִׁית בָּרָא אֱלֹהִים אֵת הָאָרֶץ׃",
         "בְּרֵאשִׁית       בָּרָא    אֱלֹהִים אֵת  הָאָרֶץ      ׃\nבְּ-רֵאשִׁית      בָּרָא    אֱלֹהִים אֵת  הָ-אָרֶץ     ׃\nin-beginning create God   ACC DEF-earth ?")
      ])
  , ("arabic",
      [ ("قرأت الكتب.",
         "قرأت         الكتب       .\nقرأ-ت        ال-كتب      .\nread-1SG.PST DEF-book.PL ?"),
        ("أنا أقرأ الكتب.",
         "أنا أقرأ         الكتب       .\nأنا أ-قرأ        ال-كتب      .\n1SG 1SG.PRS-read DEF-book.PL ?")
      ])
  , ("azerbaijani",
      [ ("İşıq kitabları oxudum.",
         "İşıq  kitabları   oxudum       .\nişıq  kitab-lar-ı oxu-du-m     .\nlight book-PL-ACC read-PST-1SG ?"),
        ("Mən evdə kitabı oxumadım.",
         "Mən evdə      kitabı   oxumadım         .\nmən ev-də     kitab-ı  oxu-ma-dı-m      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("crimean-tatar",
      [ ("Men kitaplarnı oqudım.",
         "Men kitaplarnı   oqudım       .\nmen kitap-lar-nı oqu-dı-m     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Men evde kitapnı oqumadım.",
         "Men evde      kitapnı  oqumadım         .\nmen ev-de     kitap-nı oqu-ma-dı-m      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("english",
      [ ("she walks",
         "she   walks\nshe   walk-s\n3SG.F walk-PL")
      ])
  , ("french",
      [ ("Nous lisons des livres.",
         "Nous lisons       des      livres  .\nnous lis-ons      des      livre-s .\n1PL  read-1PL.PRS INDEF.PL book-PL ?")
      ])
  , ("german",
      [ ("Ich lese Bücher.",
         "Ich lese         Bücher  .\nich les-e        büch-er .\n1SG read-1SG.PRS book-PL ?")
      ])
  , ("greek",
      [ ("Διαβάζω βιβλία.",
         "Διαβάζω      βιβλία          .\nδιαβάζ-ω     βιβλί-α         .\nread-1SG.PRS book-NOM.ACC.PL ?")
      ])
  , ("hebrew",
      [ ("קראתי את הספרים.",
         "קראתי        את  הספרים      .\nקרא-תי       את  ה-ספר-ים    .\nread-PST.1SG ACC DEF-book-PL ?"),
        ("אני קורא ספרים.",
         "אני קורא ספרים   .\nאני קורא ספר-ים  .\n1SG read book-PL ?")
      ])
  , ("italian",
      [ ("Io leggo libri.",
         "Io  leggo        libri   .\nio  legg-o       libr-i  .\n1SG read-1SG.PRS book-PL ?")
      ])
  , ("kazakh",
      [ ("Мен кітаптарды оқыдым.",
         "Мен кітаптарды   оқыдым       .\nмен кітап-тар-ды оқы-ды-м     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Мен үйде кітапты оқымадым.",
         "Мен үйде      кітапты  оқымадым         .\nмен үй-де     кітап-ты оқы-ма-ды-м      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("kyrgyz",
      [ ("Мен китептерди окудум.",
         "Мен китептерди   окудум       .\nмен китеп-тер-ди оку-ду-м     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Мен үйдө китепти окубадым.",
         "Мен үйдө      китепти  окубадым         .\nмен үй-дө     китеп-ти оку-ба-ды-м      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("latin",
      [ ("Libros lego.",
         "Libros      lego         .\nlibr-os     leg-o        .\nbook-ACC.PL read-1SG.PRS ?")
      ])
  , ("persian",
      [ ("من کتاب\8204ها را می\8204خوانم.",
         "من  کتاب\8204ها  را  می\8204خوانم       .\nمن  کتاب\8204-ها را  می\8204-خوان-م     .\n1SG book-PL ACC IPFV-read-1SG ?")
      ])
  , ("polish",
      [ ("Czytam książki.",
         "Czytam       książki     .\nczyta-m      książk-i    .\nread-1SG.PRS book-PL.ACC ?")
      ])
  , ("russian",
      [ ("Я читаю книги.",
         "Я   читаю        книги       .\nя   чита-ю       книг-и      .\n1SG read-1SG.PRS book-PL.ACC ?")
      ])
  , ("spanish",
      [ ("Yo leo libros.",
         "Yo  leo          libros    .\nyo  le-o         libr-o-s  .\n1SG read-1SG.PRS book-M-PL ?")
      ])
  , ("tajik",
      [ ("Ман китобҳоро мехонам.",
         "Ман китобҳоро   мехонам       .\nман китоб-ҳо-ро ме-хон-ам     .\n1SG book-PL-ACC IPFV-read-1SG ?")
      ])
  , ("tatar",
      [ ("мин китапларны укыдым",
         "мин китапларны   укыдым\nмин китап-лар-ны укы-ды-м\n1SG book-PL-ACC  read-PST-1SG"),
        ("мин өйдә китапны укымадым",
         "мин өйдә      китапны  укымадым\nмин өй-дә     китап-ны укы-ма-ды-м\n1SG house-LOC book-ACC read-NEG-PST-1SG")
      ])
  , ("turkish",
      [ ("Işık kitapları okudum.",
         "Işık  kitapları   okudum       .\nışık  kitap-lar-ı oku-du-m     .\nlight book-PL-ACC read-PST-1SG ?"),
        ("Evde kitabı okumadım.",
         "Evde      kitabı   okumadım         .\nev-de     kitab-ı  oku-ma-dı-m      .\nhouse-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("turkmen",
      [ ("Men kitaplary okadym.",
         "Men kitaplary   okadym       .\nmen kitap-lar-y oka-dy-m     .\n1SG book-PL-ACC read-PST-1SG ?"),
        ("Men öýde kitaby okamadym.",
         "Men öýde      kitaby   okamadym         .\nmen öý-de     kitab-y  oka-ma-dy-m      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("uzbek",
      [ ("Men kitoblarni o'qidim.",
         "Men kitoblarni   o'qidim      .\nmen kitob-lar-ni o'qi-di-m    .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Men uyda kitobni o'qimadim.",
         "Men uyda      kitobni  o'qimadim        .\nmen uy-da     kitob-ni o'qi-ma-di-m     .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  ]
