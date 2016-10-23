module Vocabulary exposing
  ( words
  , randomWord
  )

import Random
import Util
import Word exposing (Word, Kind(..), KanjiOrKana(..), exampleWord)

-- HELPER

w : Kind -> List KanjiOrKana -> Word
w kind chars =
  { kind = kind, characters = chars }

-- DATA SET

words : List Word
words =
  [ w GodanVerbEndingWithSu [ Kanji "話" "はな", Kana "す" ]
  , w GodanVerbEndingWithSu [ Kanji "返" "かえ", Kana "す" ]
  , w GodanVerbEndingWithKu [ Kanji "書" "か", Kana "く" ]
  , w GodanVerbEndingWithKu [ Kanji "着" "つ", Kana "く" ]
  , w GodanVerbEndingWithGu [ Kanji "泳" "およ", Kana "ぐ" ]
  , w GodanVerbEndingWithGu [ Kanji "注" "そそ", Kana "ぐ" ]
  , w GodanVerbEndingWithMu [ Kanji "飲" "の", Kana "む" ]
  , w GodanVerbEndingWithMu [ Kanji "読" "よ", Kana "む" ]
  , w GodanVerbEndingWithBu [ Kanji "遊" "あそ", Kana "ぶ" ]
  , w GodanVerbEndingWithBu [ Kanji "呼" "よ", Kana "ぶ" ]
  , w GodanVerbEndingWithRu [ Kanji "帰" "かえ", Kana "る" ]
  , w GodanVerbEndingWithRu [ Kanji "走" "はし", Kana "る" ]
  , w GodanVerbEndingWithRu [ Kanji "止" "と", Kana "まる" ]
  , w GodanVerbEndingWithU [ Kanji "買" "か", Kana "う" ]
  , w GodanVerbEndingWithU [ Kanji "吸" "す", Kana "う" ]
  , w GodanVerbEndingWithTsu [ Kanji "待" "ま", Kana "つ" ]
  , w GodanVerbEndingWithTsu [ Kanji "持" "も", Kana "つ" ]
  , w GodanVerbEndingWithTsu [ Kanji "立" "た", Kana "つ" ]
  , w IchidanVerb [ Kanji "食" "た", Kana "べる" ]
  , w IchidanVerb [ Kanji "見" "み", Kana "つける" ]
  , w GodanVerbEndingWithNu [ Kanji "死" "し", Kana "ぬ" ]
  , w GodanVerbIkuYukuClass [ Kanji "行" "い", Kana "く" ]
  , w SahenVerb [ Kana "する" ]
  , w KahenVerb [ Kanji "来" "く", Kana "る" ]
  ]

randomWord : Random.Generator Word
randomWord =
  Util.sample words
