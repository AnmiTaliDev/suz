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
  [ ("afrikaans",
      [ ("Ek lees boeke.",
         "Ek  lees boeke   .\nek  lees boek-e  .\n1SG read book-PL ?"),
        ("Ek lees die boek.",
         "Ek  lees die boek .\nek  lees die boek .\n1SG read DEF book ?")
      ])
  , ("ancient-hebrew",
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
  , ("bashkir",
      [ ("Мин китаптарҙы уҡыным.",
         "Мин китаптарҙы   уҡыным       .\nмин китап-тар-ҙы уҡы-ны-м     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Мин өйҙә китапты уҡыманым.",
         "Мин өйҙә      китапты  уҡыманым         .\nмин өй-ҙә     китап-ты уҡы-ма-ны-м      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("belarusian",
      [ ("Я чытаю кнігі.",
         "Я   чытаю        кнігі       .\nя   чыта-ю       кніг-і      .\n1SG read-1SG.PRS book-PL.ACC ?"),
        ("Мы чытаем кнігі.",
         "Мы  чытаем       кнігі       .\nмы  чыта-ем      кніг-і      .\n1PL read-1PL.PRS book-PL.ACC ?")
      ])
  , ("bulgarian",
      [ ("Аз чета книги.",
         "Аз  чета         книги   .\nаз  чет-а        книг-и  .\n1SG read-1SG.PRS book-PL ?"),
        ("Ние четем книгите.",
         "Ние четем        книгите     .\nние чет-ем       книг-и-те   .\n1PL read-1PL.PRS book-PL-DEF ?")
      ])
  , ("catalan",
      [ ("Llegeixo llibres.",
         "Llegeixo     llibres  .\nlleg-eixo    llibre-s .\nread-1SG.PRS book-PL  ?"),
        ("Llegim llibres.",
         "Llegim       llibres  .\nlleg-im      llibre-s .\nread-1PL.PRS book-PL  ?")
      ])
  , ("chuvash",
      [ ("Эпӗ кӗнекесене вуларӑм.",
         "Эпӗ кӗнекесене  вуларӑм      .\nэпӗ кӗнеке-сене вула-р-ӑм    .\n1SG book-PL.ACC read-PST-1SG ?"),
        ("Эпӗ ҫуртра кӗнеке вуламарӑм.",
         "Эпӗ ҫуртра    кӗнеке вуламарӑм        .\nэпӗ ҫурт-ра   кӗнеке вула-ма-р-ӑм     .\n1SG house-LOC book   read-NEG-PST-1SG ?")
      ])
  , ("crimean-tatar",
      [ ("Men kitaplarnı oqudım.",
         "Men kitaplarnı   oqudım       .\nmen kitap-lar-nı oqu-dı-m     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Men evde kitapnı oqumadım.",
         "Men evde      kitapnı  oqumadım         .\nmen ev-de     kitap-nı oqu-ma-dı-m      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("croatian",
      [ ("Čitam knjige.",
         "Čitam        knjige      .\nčita-m       knjig-e     .\nread-1SG.PRS book-PL.ACC ?"),
        ("Čitamo knjige.",
         "Čitamo       knjige      .\nčita-mo      knjig-e     .\nread-1PL.PRS book-PL.ACC ?")
      ])
  , ("czech",
      [ ("Čtu knihy.",
         "Čtu          knihy       .\nčt-u         knih-y      .\nread-1SG.PRS book-PL.ACC ?"),
        ("Čteme knihy.",
         "Čteme        knihy       .\nčt-eme       knih-y      .\nread-1PL.PRS book-PL.ACC ?")
      ])
  , ("danish",
      [ ("Jeg læser bøger.",
         "Jeg læser    bøger   .\njeg læs-er   bøg-er  .\n1SG read-PRS book-PL ?"),
        ("Jeg læste bogen.",
         "Jeg læste    bogen    .\njeg læs-te   bog-en   .\n1SG read-PST book-DEF ?")
      ])
  , ("dutch",
      [ ("Ik lees boeken.",
         "Ik  lees boeken  .\nik  lees boek-en .\n1SG read book-PL ?"),
        ("Ik las het boek.",
         "Ik  las      het boek .\nik  las      het boek .\n1SG read.PST DEF book ?")
      ])
  , ("english",
      [ ("she walks",
         "she   walks\nshe   walk-s\n3SG.F walk-PL")
      ])
  , ("esperanto",
      [ ("Mi legas librojn.",
         "Mi  legas    librojn       .\nmi  leg-as   libr-o-j-n    .\n1SG read-PRS book-N-PL-ACC ?"),
        ("Ni legis librojn.",
         "Ni  legis    librojn       .\nni  leg-is   libr-o-j-n    .\n1PL read-PST book-N-PL-ACC ?")
      ])
  , ("estonian",
      [ ("Ma lugesin raamatuid majas.",
         "Ma  lugesin      raamatuid   majas     .\nma  luge-si-n    raamatu-id  maja-s    .\n1SG read-PST-1SG book-PL.PTV house-INE ?"),
        ("Ma lugesin raamatut.",
         "Ma  lugesin      raamatut  .\nma  luge-si-n    raamatu-t .\n1SG read-PST-1SG book-PTV  ?")
      ])
  , ("finnish",
      [ ("Luin kirjan talossa.",
         "Luin         kirjan   talossa   .\nlu-i-n       kirja-n  talo-ssa  .\nread-PST-1SG book-ACC house-INE ?"),
        ("Luen kirjoja.",
         "Luen     kirjoja     .\nlue-n    kirjo-j-a   .\nread-1SG book-PL-PTV ?")
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
  , ("hungarian",
      [ ("Könyveket olvasok.",
         "Könyveket   olvasok      .\nkönyv-ek-et olvas-ok     .\nbook-PL-ACC read-1SG.PRS ?"),
        ("Olvastam a könyvet.",
         "Olvastam     a   könyvet  .\nolvas-t-am   a   könyv-et .\nread-PST-1SG DEF book-ACC ?"),
        ("Házban olvastam.",
         "Házban    olvastam     .\nház-ban   olvas-t-am   .\nhouse-INE read-PST-1SG ?")
      ])
  , ("indonesian",
      [ ("Saya membaca buku.",
         "Saya membaca  buku .\nsaya mem-baca buku .\n1SG  AV-read  book ?"),
        ("Kami membaca buku di rumah.",
         "Kami membaca  buku di  rumah .\nkami mem-baca buku di  rumah .\n1PL  AV-read  book LOC house ?")
      ])
  , ("italian",
      [ ("Io leggo libri.",
         "Io  leggo        libri   .\nio  legg-o       libr-i  .\n1SG read-1SG.PRS book-PL ?")
      ])
  , ("karakalpak",
      [ ("Men kitaplardı oqıdım.",
         "Men kitaplardı   oqıdım       .\nmen kitap-lar-dı oqı-dı-m     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Men úyde kitaptı oqımadım.",
         "Men úyde      kitaptı  oqımadım         .\nmen úy-de     kitap-tı oqı-ma-dı-m      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("kazakh",
      [ ("Мен кітаптарды оқыдым.",
         "Мен кітаптарды   оқыдым       .\nмен кітап-тар-ды оқы-ды-м     .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Мен үйде кітапты оқымадым.",
         "Мен үйде      кітапты  оқымадым         .\nмен үй-де     кітап-ты оқы-ма-ды-м      .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("korean",
      [ ("나는 책을 읽었다.",
         "나는    책을     읽었다        .\n나-는   책-을    읽-었-다      .\n1SG-TOP book-ACC read-PST-DECL ?"),
        ("나는 학교에서 책을 읽었다.",
         "나는    학교에서   책을     읽었다        .\n나-는   학교-에서  책-을    읽-었-다      .\n1SG-TOP school-LOC book-ACC read-PST-DECL ?")
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
  , ("latvian",
      [ ("Es lasu grāmatas.",
         "Es  lasu         grāmatas  .\nes  las-u        grāmat-as .\n1SG read-1SG.PRS book-PL   ?"),
        ("Mēs lasām grāmatas.",
         "Mēs lasām        grāmatas  .\nmēs las-ām       grāmat-as .\n1PL read-1PL.PRS book-PL   ?")
      ])
  , ("mongolian",
      [ ("Би номуудыг уншсан.",
         "Би  номуудыг    уншсан   .\nби  ном-ууд-ыг  унш-сан  .\n1SG book-PL-ACC read-PST ?"),
        ("Би номыг уншсан.",
         "Би  номыг    уншсан   .\nби  ном-ыг   унш-сан  .\n1SG book-ACC read-PST ?")
      ])
  , ("persian",
      [ ("من کتاب\8204ها را می\8204خوانم.",
         "من  کتاب\8204ها  را  می\8204خوانم       .\nمن  کتاب\8204-ها را  می\8204-خوان-م     .\n1SG book-PL ACC IPFV-read-1SG ?")
      ])
  , ("polish",
      [ ("Czytam książki.",
         "Czytam       książki     .\nczyta-m      książk-i    .\nread-1SG.PRS book-PL.ACC ?")
      ])
  , ("portuguese",
      [ ("Eu leio livros.",
         "Eu  leio         livros    .\neu  le-io        livr-o-s  .\n1SG read-1SG.PRS book-M-PL ?"),
        ("Nós lemos livros.",
         "Nós lemos        livros    .\nnós le-mos       livr-o-s  .\n1PL read-1PL.PRS book-M-PL ?")
      ])
  , ("russian",
      [ ("Я читаю книги.",
         "Я   читаю        книги       .\nя   чита-ю       книг-и      .\n1SG read-1SG.PRS book-PL.ACC ?")
      ])
  , ("sakha",
      [ ("Мин кинигэлэри ааҕабын.",
         "Мин кинигэлэри   ааҕабын      .\nмин кинигэ-лэр-и ааҕ-а-бын    .\n1SG book-PL-ACC  read-PRS-1SG ?"),
        ("Мин кинигэни ааҕабын.",
         "Мин кинигэни  ааҕабын      .\nмин кинигэ-ни ааҕ-а-бын    .\n1SG book-ACC  read-PRS-1SG ?")
      ])
  , ("spanish",
      [ ("Yo leo libros.",
         "Yo  leo          libros    .\nyo  le-o         libr-o-s  .\n1SG read-1SG.PRS book-M-PL ?")
      ])
  , ("swahili",
      [ ("Ninasoma vitabu.",
         "Ninasoma     vitabu  .\nni-na-soma   vi-tabu .\n1SG-PRS-read PL-book ?"),
        ("Nilisoma kitabu.",
         "Nilisoma     kitabu  .\nni-li-soma   ki-tabu .\n1SG-PST-read SG-book ?")
      ])
  , ("swedish",
      [ ("Jag läser böcker.",
         "Jag läser    böcker  .\njag läs-er   böck-er .\n1SG read-PRS book-PL ?"),
        ("Jag läste boken.",
         "Jag läste    boken    .\njag läs-te   bok-en   .\n1SG read-PST book-DEF ?")
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
  , ("ukrainian",
      [ ("Я читаю книги.",
         "Я   читаю        книги       .\nя   чита-ю       книг-и      .\n1SG read-1SG.PRS book-PL.ACC ?"),
        ("Ми читаємо книги.",
         "Ми  читаємо      книги       .\nми  чита-ємо     книг-и      .\n1PL read-1PL.PRS book-PL.ACC ?")
      ])
  , ("uyghur",
      [ ("مەن كىتابلارنى ئوقۇدۇم.",
         "مەن كىتابلارنى   ئوقۇدۇم      .\nمەن كىتاب-لار-نى ئوقۇ-دۇ-م    .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("مەن ئۆيدە كىتابنى ئوقۇمىدىم.",
         "مەن ئۆيدە     كىتابنى  ئوقۇمىدىم        .\nمەن ئۆي-دە    كىتاب-نى ئوقۇ-مى-دى-م     .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  , ("uzbek",
      [ ("Men kitoblarni o'qidim.",
         "Men kitoblarni   o'qidim      .\nmen kitob-lar-ni o'qi-di-m    .\n1SG book-PL-ACC  read-PST-1SG ?"),
        ("Men uyda kitobni o'qimadim.",
         "Men uyda      kitobni  o'qimadim        .\nmen uy-da     kitob-ni o'qi-ma-di-m     .\n1SG house-LOC book-ACC read-NEG-PST-1SG ?")
      ])
  ]
