module Conjugation exposing
  ( Form(..)
  , exampleForm
  , randomForm
  , conjugate
  )

import String
import Random
import Word exposing (Word, KanjiOrKana(..), Kind(..))

type Form
  = Present
  | Past
  | Te
  | Masu
  | Negative

exampleForm : Form
exampleForm = Negative

randomForm : Random.Generator Form
randomForm =
  let
    pickForm i =
      case i of
        1 -> Present
        2 -> Past
        3 -> Te
        4 -> Masu
        5 -> Negative
        _ -> exampleForm -- this should never happen ;)
  in
    Random.map pickForm (Random.int 1 5)

conjugate : Form -> Word -> Result String Word
conjugate form word =
  let
    applyRules : (List KanjiOrKana -> Result String (List KanjiOrKana))
    applyRules =
      case word.kind of
        GodanVerbEndingWithKu ->
          case form of
            Present -> Ok
            Past -> replaceLastCharacterWith "いた"
            Te -> replaceLastCharacterWith "いて"
            Masu -> replaceLastCharacterWith "きます"
            Negative -> replaceLastCharacterWith "かない"

        GodanVerbEndingWithMu ->
          case form of
            Present -> Ok
            Past -> replaceLastCharacterWith "んだ"
            Te -> replaceLastCharacterWith "んで"
            Masu -> replaceLastCharacterWith "みます"
            Negative -> replaceLastCharacterWith "まない"

        GodanVerbEndingWithRu ->
          case form of
            Present -> Ok
            Past -> replaceLastCharacterWith "った"
            Te -> replaceLastCharacterWith "って"
            Masu -> replaceLastCharacterWith "ります"
            Negative -> replaceLastCharacterWith "らない"

        IchidanVerb ->
          case form of
            Present -> Ok
            Past -> replaceLastCharacterWith "た"
            Te -> replaceLastCharacterWith "て"
            Masu -> replaceLastCharacterWith "ます"
            Negative -> replaceLastCharacterWith "ない"

        Conjugated ->
          \_ -> Err "Trying to conjugate an already conjugated word"
  in
    case applyRules word.characters of
      Err e -> Err e
      Ok chars -> Ok { kind = Conjugated, characters = chars }

replaceLastCharacterWith : String -> List KanjiOrKana -> Result String (List KanjiOrKana)
replaceLastCharacterWith newStr characters =
  case dropLastCharacter characters of
    Err e -> Err e
    Ok lastCharDropped -> Ok <| appendCharacters newStr lastCharDropped

dropLastCharacter : List KanjiOrKana -> Result String (List KanjiOrKana)
dropLastCharacter characters =
  let
    reversed = List.reverse characters
  in
    case List.head reversed of
      Nothing -> Err "Cannot drop characters from an empty word"
      Just (Kanji _ _) -> Err "Cannot drop characters from Kanji"
      Just (Kana "") -> Err "Cannot drop characters from empty Kana"
      Just (Kana k) ->
        k |> String.dropRight 1
          |> Kana
          |> (flip (::) <| List.drop 1 reversed)
          |> List.reverse
          |> Ok

appendCharacters : String -> List KanjiOrKana -> List KanjiOrKana
appendCharacters newChars characters =
  let
    reversed = List.reverse characters
  in
    case List.head reversed of
      Nothing -> [ Kana newChars ]
      Just (Kanji _ _) -> characters ++ [ Kana newChars ]
      Just (Kana k) ->
        k ++ newChars
          |> Kana
          |> (flip (::) <| List.drop 1 reversed)
          |> List.reverse
