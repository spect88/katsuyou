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
        GodanVerbEndingWithSu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "した"
            Te -> replaceLastCharacterWith "して"
            Masu -> replaceLastCharacterWith "します"
            Negative -> replaceLastCharacterWith "さない"
            NegativePast -> replaceLastCharacterWith "さなかった"
            Potential -> replaceLastCharacterWith "せる"
            Passive -> replaceLastCharacterWith "される"
            Imperative -> replaceLastCharacterWith "せ"
            Volitional -> replaceLastCharacterWith "そう"
            Conditional -> replaceLastCharacterWith "せば"
            Causative -> replaceLastCharacterWith "させる"
            CausativePassive -> replaceLastCharacterWith "させられる"

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

        GodanVerbEndingWithGu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "いだ"
            Te -> replaceLastCharacterWith "いで"
            Masu -> replaceLastCharacterWith "ぎます"
            Negative -> replaceLastCharacterWith "がない"
            NegativePast -> replaceLastCharacterWith "がなかった"
            Potential -> replaceLastCharacterWith "げる"
            Passive -> replaceLastCharacterWith "がれる"
            Imperative -> replaceLastCharacterWith "げ"
            Volitional -> replaceLastCharacterWith "ごう"
            Conditional -> replaceLastCharacterWith "げば"
            Causative -> replaceLastCharacterWith "がせる"
            CausativePassive -> replaceLastCharacterWith "がされる"

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

        GodanVerbEndingWithBu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "んだ"
            Te -> replaceLastCharacterWith "んで"
            Masu -> replaceLastCharacterWith "びます"
            Negative -> replaceLastCharacterWith "ばない"
            NegativePast -> replaceLastCharacterWith "ばなかった"
            Potential -> replaceLastCharacterWith "べる"
            Passive -> replaceLastCharacterWith "ばれる"
            Imperative -> replaceLastCharacterWith "べ"
            Volitional -> replaceLastCharacterWith "ぼう"
            Conditional -> replaceLastCharacterWith "べば"
            Causative -> replaceLastCharacterWith "ばせる"
            CausativePassive -> replaceLastCharacterWith "ばされる"

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

        GodanVerbEndingWithU ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "った"
            Te -> replaceLastCharacterWith "って"
            Masu -> replaceLastCharacterWith "います"
            Negative -> replaceLastCharacterWith "わない"
            NegativePast -> replaceLastCharacterWith "わなかった"
            Potential -> replaceLastCharacterWith "える"
            Passive -> replaceLastCharacterWith "われる"
            Imperative -> replaceLastCharacterWith "え"
            Volitional -> replaceLastCharacterWith "おう"
            Conditional -> replaceLastCharacterWith "えば"
            Causative -> replaceLastCharacterWith "わせる"
            CausativePassive -> replaceLastCharacterWith "わされる"

        GodanVerbEndingWithTsu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "った"
            Te -> replaceLastCharacterWith "って"
            Masu -> replaceLastCharacterWith "ちます"
            Negative -> replaceLastCharacterWith "たない"
            NegativePast -> replaceLastCharacterWith "たなかった"
            Potential -> replaceLastCharacterWith "てる"
            Passive -> replaceLastCharacterWith "たれる"
            Imperative -> replaceLastCharacterWith "て"
            Volitional -> replaceLastCharacterWith "とう"
            Conditional -> replaceLastCharacterWith "てば"
            Causative -> replaceLastCharacterWith "たせる"
            CausativePassive -> replaceLastCharacterWith "たされる"

        GodanVerbEndingWithNu ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "んだ"
            Te -> replaceLastCharacterWith "んで"
            Masu -> replaceLastCharacterWith "にます"
            Negative -> replaceLastCharacterWith "なない"
            NegativePast -> replaceLastCharacterWith "ななかった"
            Potential -> replaceLastCharacterWith "ねる"
            Passive -> replaceLastCharacterWith "なれる"
            Imperative -> replaceLastCharacterWith "ね"
            Volitional -> replaceLastCharacterWith "のう"
            Conditional -> replaceLastCharacterWith "ねば"
            Causative -> replaceLastCharacterWith "なせる"
            CausativePassive -> replaceLastCharacterWith "なされる"

        GodanVerbIkuYukuClass ->
          case form of
            Basic -> Ok
            Past -> replaceLastCharacterWith "った"
            Te -> replaceLastCharacterWith "って"
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

        SahenVerb ->
          case form of
            Basic -> Ok
            Past -> replaceSuruWith "した"
            Te -> replaceSuruWith "して"
            Masu -> replaceSuruWith "します"
            Negative -> replaceSuruWith "しない"
            NegativePast -> replaceSuruWith "しなかった"
            Potential -> replaceSuruWith "できる"
            Passive -> replaceSuruWith "される"
            Imperative -> replaceSuruWith "しろ"
            Volitional -> replaceSuruWith "しよう"
            Conditional -> replaceSuruWith "すれば"
            Causative -> replaceSuruWith "させる"
            CausativePassive -> replaceSuruWith "させられる"

        KahenVerb ->
          case form of
            Basic -> Ok
            Past -> replaceKuruWith "き" "た"
            Te -> replaceKuruWith "き" "て"
            Masu -> replaceKuruWith "き" "ます"
            Negative -> replaceKuruWith "こ" "ない"
            NegativePast -> replaceKuruWith "こ" "なかった"
            Potential -> replaceKuruWith "こ" "られる"
            Passive -> replaceKuruWith "こ" "られる"
            Imperative -> replaceKuruWith "こ" "い"
            Volitional -> replaceKuruWith "こ" "よう"
            Conditional -> replaceKuruWith "く" "れば"
            Causative -> replaceKuruWith "こ" "させる"
            CausativePassive -> replaceKuruWith "こ" "させられる"

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

replaceSuruWith : String -> List KanjiOrKana -> Result String (List KanjiOrKana)
replaceSuruWith newStr characters =
  let
    withoutSuru =
      dropLastCharacter characters `Result.andThen` dropLastCharacter
  in
    withoutSuru
      |> Result.map (flip (++) <| [ Kana newStr ])

replaceKuruWith : String -> String -> List KanjiOrKana -> Result String (List KanjiOrKana)
replaceKuruWith kanjiReading kanaEnding characters =
  characters
    |> List.reverse
    |> List.drop 2 -- assuming 来 and る being 2 last elements
    |> List.reverse
    |> (flip (++) <| [ Kanji "来" kanjiReading, Kana kanaEnding ])
    |> Ok


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
