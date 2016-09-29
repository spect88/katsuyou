module Conjugation exposing
  ( Form(..)
  , exampleForm
  , randomForm
  , formDescription
  , conjugate
  )

import String
import Random
import Util
import Word exposing (Word, KanjiOrKana(..), Kind(..))

type Form
  = Basic
  | Past
  | Te
  | Masu
  | Negative
  | NegativePast
  | Potential
  | Passive
  | Imperative
  | Volitional
  | Conditional
  | Causative
  | CausativePassive

allForms : List Form
allForms =
  [ Basic
  , Past
  , Te
  , Masu
  , Negative
  , NegativePast
  , Potential
  , Passive
  , Imperative
  , Volitional
  , Conditional
  , Causative
  , CausativePassive
  ]

formDescription : Form -> String
formDescription form =
  case form of
    Basic -> "basic (dictionary)"
    Past -> "past"
    Te -> "te"
    Masu -> "polite (masu)"
    Negative -> "negative"
    NegativePast -> "past negative"
    Potential -> "potential (can, be able to)"
    Passive -> "passive"
    Imperative -> "imperative"
    Volitional -> "volitional (let's)"
    Conditional -> "conditional (if)"
    Causative -> "causative (make someone do)"
    CausativePassive -> "causative passive (be forced to)"

exampleForm : Form
exampleForm = Negative

randomForm : Random.Generator Form
randomForm =
  Util.sample allForms

conjugate : Form -> Word -> Result String Word
conjugate form word =
  let
    applyRules : (List KanjiOrKana -> Result String (List KanjiOrKana))
    applyRules =
      case word.kind of
        GodanVerbEndingWithKu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "いた"
            Te -> replaceLastCharacterWith "いて"
            Masu -> replaceLastCharacterWith "きます"
            Negative -> replaceLastCharacterWith "かない"
            NegativePast -> replaceLastCharacterWith "かなかった"
            Potential -> replaceLastCharacterWith "ける"
            Passive -> replaceLastCharacterWith "かれる"
            Imperative -> replaceLastCharacterWith "け"
            Volitional -> replaceLastCharacterWith "こう"
            Conditional -> replaceLastCharacterWith "けば"
            Causative -> replaceLastCharacterWith "かせる"
            CausativePassive -> replaceLastCharacterWith "かされる"

        GodanVerbEndingWithMu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "んだ"
            Te -> replaceLastCharacterWith "んで"
            Masu -> replaceLastCharacterWith "みます"
            Negative -> replaceLastCharacterWith "まない"
            NegativePast -> replaceLastCharacterWith "まなかった"
            Potential -> replaceLastCharacterWith "める"
            Passive -> replaceLastCharacterWith "まれる"
            Imperative -> replaceLastCharacterWith "め"
            Volitional -> replaceLastCharacterWith "もう"
            Conditional -> replaceLastCharacterWith "めば"
            Causative -> replaceLastCharacterWith "ませる"
            CausativePassive -> replaceLastCharacterWith "まされる"

        GodanVerbEndingWithRu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "った"
            Te -> replaceLastCharacterWith "って"
            Masu -> replaceLastCharacterWith "ります"
            Negative -> replaceLastCharacterWith "らない"
            NegativePast -> replaceLastCharacterWith "らなかった"
            Potential -> replaceLastCharacterWith "れる"
            Passive -> replaceLastCharacterWith "られる"
            Imperative -> replaceLastCharacterWith "れ"
            Volitional -> replaceLastCharacterWith "ろう"
            Conditional -> replaceLastCharacterWith "れば"
            Causative -> replaceLastCharacterWith "らせる"
            CausativePassive -> replaceLastCharacterWith "らされる"

        IchidanVerb ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "た"
            Te -> replaceLastCharacterWith "て"
            Masu -> replaceLastCharacterWith "ます"
            Negative -> replaceLastCharacterWith "ない"
            NegativePast -> replaceLastCharacterWith "なかった"
            Potential -> replaceLastCharacterWith "られる"
            Passive -> replaceLastCharacterWith "られる"
            Imperative -> replaceLastCharacterWith "ろ"
            Volitional -> replaceLastCharacterWith "よう"
            Conditional -> replaceLastCharacterWith "れば"
            Causative -> replaceLastCharacterWith "させる"
            CausativePassive -> replaceLastCharacterWith "させられる"

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
