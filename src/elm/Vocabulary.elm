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
  , w GodanVerbEndingWithKu [ Kanji "書" "か", Kana "く" ]
  , w GodanVerbEndingWithGu [ Kanji "泳" "およ", Kana "ぐ" ]
  , w GodanVerbEndingWithMu [ Kanji "飲" "の", Kana "む" ]
  , w GodanVerbEndingWithBu [ Kanji "遊" "あそ", Kana "ぶ" ]
  , w GodanVerbEndingWithRu [ Kanji "帰" "かえ", Kana "る" ]
  , w GodanVerbEndingWithU [ Kanji "買" "か", Kana "う" ]
  , w GodanVerbEndingWithTsu [ Kanji "待" "ま", Kana "つ" ]
  , w IchidanVerb [ Kanji "食" "た", Kana "べる" ]
  , w GodanVerbEndingWithNu [ Kanji "死" "し", Kana "ぬ" ]
  , w GodanVerbIkuYukuClass [ Kanji "行" "い", Kana "く" ]
  , w SahenVerb [ Kana "する" ]
  , w KahenVerb [ Kanji "来" "く", Kana "る" ]
  ]

randomWord : Random.Generator Word
randomWord =
  Util.sample words
