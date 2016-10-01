module Word exposing
  ( Word
  , KanjiOrKana(..)
  , Kind(..)
  , toKanji
  , toRuby
  , toKana
  , toRomaji
  , exampleWord
  )

import Html exposing (Html, ruby, rt, rp, span, text)
import List as L
import String as String
import Romkan exposing (toRoma)

type alias Kana = String
type alias Furigana = Kana

type KanjiOrKana
  = Kana Kana
  | Kanji String Furigana

type Kind
  = GodanVerbEndingWithSu
  | GodanVerbEndingWithKu
  | GodanVerbEndingWithGu
  | GodanVerbEndingWithMu
  | GodanVerbEndingWithBu
  | GodanVerbEndingWithNu
  | GodanVerbEndingWithRu
  | GodanVerbEndingWithU
  | GodanVerbEndingWithTsu
  | GodanVerbIkuYukuClass
  | IchidanVerb
  | SahenVerb
  | KahenVerb
  | Conjugated

type alias Word =
  { kind: Kind
  , characters: List KanjiOrKana
  }

exampleWord : Word
exampleWord =
  { kind = GodanVerbEndingWithKu
  , characters = [ Kanji "書" "か", Kana "く" ]
  }

toKanji : Word -> String
toKanji word =
  let
    toKanji' kanjiOrKana =
      case kanjiOrKana of
        Kanji kanji _ -> kanji
        Kana kana -> kana
  in
    word.characters
      |> L.map toKanji'
      |> String.join ""

toRuby : Word -> Html msg
toRuby word =
  let
    toRuby' kanjiOrKana =
      case kanjiOrKana of
        Kanji kanji furigana ->
          [ span [] [ text kanji ]
          , rp [] [ text "(" ]
          , rt [] [ text furigana ]
          , rp [] [ text ")" ]
          ]
        Kana kana ->
          [ span [] [ text kana ] ]
  in
    word.characters
      |> L.map toRuby'
      |> L.concat
      |> ruby []

toKana : Word -> Kana
toKana word =
  let
    toKana' kanjiOrKana =
      case kanjiOrKana of
        Kanji _ furigana ->
          furigana
        Kana kana -> kana
  in
    word.characters
      |> L.map toKana'
      |> String.join ""

toRomaji : Word -> String
toRomaji word =
  toRoma <| toKana word
