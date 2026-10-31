-- SPDX-License-Identifier: GPL-3.0-or-later
module Test.Languages (checks) where

import Control.Monad (foldM)
import Data.Text (Text)
import Fixtures
import Suz
import Suz.Languages.Azerbaijani (azerbaijani)
import Suz.Languages.CrimeanTatar (crimeanTatar)
import Suz.Languages.English (english)
import Suz.Languages.French (french)
import Suz.Languages.German (german)
import Suz.Languages.Kazakh (kazakh)
import Suz.Languages.Kyrgyz (kyrgyz)
import Suz.Languages.Russian (russian)
import Suz.Languages.Spanish (spanish)
import Suz.Languages.Tatar (tatar)
import Suz.Languages.Turkish (turkish)
import Suz.Languages.Turkmen (turkmen)
import Suz.Languages.Uzbek (uzbek)

checks :: [(String, Bool)]
checks =
  [ ("Tatar example is annotated", annotated tatar "мин китапларны укыдым" == "мин китапларны   укыдым\nмин китап-лар-ны укы-ды-м\n1SG book-PL-ACC  read-PST-1SG")
  , ("Tatar example passes validation", null (validateDocument tatar (annotatedDocument tatar "мин китапларны укыдым")))
  , ("English example is annotated", annotated english "she walks" == "she   walks\nshe   walk-s\n3SG.F walk-PL")
  , ("English example passes validation", null (validateDocument english (annotatedDocument english "she walks")))
  , ("Turkish example is annotated", annotated turkish "Işık kitapları okudum." == "Işık  kitapları   okudum       .\nışık  kitap-lar-ı oku-du-m     .\nlight book-PL-ACC read-PST-1SG ?")
  , ("Turkish example passes validation", null (validateDocument turkish (annotatedDocument turkish "Işık kitapları okudum.")))
  , ("Kazakh example is annotated", annotated kazakh "Мен кітаптарды оқыдым." == "Мен кітаптарды   оқыдым       .\nмен кітап-тар-ды оқы-ды-м     .\n1SG book-PL-ACC  read-PST-1SG ?")
  , ("Kazakh example passes validation", null (validateDocument kazakh (annotatedDocument kazakh "Мен кітаптарды оқыдым.")))
  , ("Kyrgyz example is annotated", annotated kyrgyz "Мен китептерди окудум." == "Мен китептерди   окудум       .\nмен китеп-тер-ди оку-ду-м     .\n1SG book-PL-ACC  read-PST-1SG ?")
  , ("Kyrgyz example passes validation", null (validateDocument kyrgyz (annotatedDocument kyrgyz "Мен китептерди окудум.")))
  , ("Uzbek example is annotated", annotated uzbek "Men kitoblarni o'qidim." == "Men kitoblarni   o'qidim      .\nmen kitob-lar-ni o'qi-di-m    .\n1SG book-PL-ACC  read-PST-1SG ?")
  , ("Uzbek example passes validation", null (validateDocument uzbek (annotatedDocument uzbek "Men kitoblarni o'qidim.")))
  , ("Azerbaijani example is annotated", annotated azerbaijani "İşıq kitabları oxudum." == "İşıq  kitabları   oxudum       .\nişıq  kitab-lar-ı oxu-du-m     .\nlight book-PL-ACC read-PST-1SG ?")
  , ("Azerbaijani example passes validation", null (validateDocument azerbaijani (annotatedDocument azerbaijani "İşıq kitabları oxudum.")))
  , ("CrimeanTatar example is annotated", annotated crimeanTatar "Men kitaplarnı oqudım." == "Men kitaplarnı   oqudım       .\nmen kitap-lar-nı oqu-dı-m     .\n1SG book-PL-ACC  read-PST-1SG ?")
  , ("CrimeanTatar example passes validation", null (validateDocument crimeanTatar (annotatedDocument crimeanTatar "Men kitaplarnı oqudım.")))
  , ("Russian example is annotated", annotated russian "Я читаю книги." == "Я   читаю        книги       .\nя   чита-ю       книг-и      .\n1SG read-1SG.PRS book-PL.ACC ?")
  , ("Russian example passes validation", null (validateDocument russian (annotatedDocument russian "Я читаю книги.")))
  , ("Turkmen example is annotated", annotated turkmen "Men kitaplary okadym." == "Men kitaplary   okadym       .\nmen kitap-lar-y oka-dy-m     .\n1SG book-PL-ACC read-PST-1SG ?")
  , ("Turkmen example passes validation", null (validateDocument turkmen (annotatedDocument turkmen "Men kitaplary okadym.")))
  , ("Spanish example is annotated", annotated spanish "Yo leo libros." == "Yo  leo          libros    .\nyo  le-o         libr-o-s  .\n1SG read-1SG.PRS book-M-PL ?")
  , ("Spanish example passes validation", null (validateDocument spanish (annotatedDocument spanish "Yo leo libros.")))
  , ("German example is annotated", annotated german "Ich lese Bücher." == "Ich lese         Bücher  .\nich les-e        büch-er .\n1SG read-1SG.PRS book-PL ?")
  , ("German example passes validation", null (validateDocument german (annotatedDocument german "Ich lese Bücher.")))
  , ("French example is annotated", annotated french "Nous lisons des livres." == "Nous lisons       des      livres  .\nnous lis-ons      des      livre-s .\n1PL  read-1PL.PRS INDEF.PL book-PL ?")
  , ("French example passes validation", null (validateDocument french (annotatedDocument french "Nous lisons des livres.")))
  ]
  where
    annotatedDocument :: Language -> Text -> Document
    annotatedDocument lang text = sessionDocument glossed
      where
        imported = newSession (importTextWith lang text)
        segmented = must (foldM (flip perform) imported (suggestSegmentation lang (sessionDocument imported)))
        glossed = must (foldM (flip perform) segmented (suggestGlosses lang (sessionDocument segmented)))

    annotated :: Language -> Text -> Text
    annotated lang text = renderDocument (annotatedDocument lang text)
