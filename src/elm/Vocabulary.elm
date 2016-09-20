module Vocabulary exposing
  ( words
  , randomWord
  )

import Random
import Word exposing (Word, Kind(..), KanjiOrKana(..), exampleWord)

-- HELPER

w : Kind -> List KanjiOrKana -> Word
w kind chars =
  { kind = kind, characters = chars }

-- DATA SET

words : List Word
words =
  [ w GodanVerbEndingWithKu [ Kanji "書" "か", Kana "く" ]
  , w GodanVerbEndingWithMu [ Kanji "飲" "の", Kana "む" ]
  , w GodanVerbEndingWithRu [ Kanji "帰" "かえ", Kana "る" ]
  , w IchidanVerb [ Kanji "食" "た", Kana "べる" ]
  ]

randomWord : Random.Generator Word
randomWord =
  let
    pickWord i =
      case words |> List.drop i |> List.head of
        Just word -> word
        Nothing -> Word.exampleWord -- this should never happen ;)
  in
    Random.map pickWord <| Random.int 0 (List.length words - 1)
