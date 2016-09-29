module Card exposing
  ( Card
  , State(..)
  , Msg
  , view
  , update
  , exampleCard
  , randomCard
  , subscriptions
  , generateNextCard
  )

import Random
import Html exposing (Html, div, p, span, h2, text, input, label, form)
import Html.Attributes exposing (value, class, classList, readonly)
import Html.Events exposing (onInput, onSubmit)

import Settings exposing (Settings, OutputFormat(..), InputFormat(..))
import Word exposing (Word, exampleWord, toKanji, toKana, toRuby, toRomaji)
import Vocabulary exposing (randomWord)
import Conjugation exposing (Form, exampleForm, formDescription, randomForm, conjugate)
import Timer

-- MODEL

type State
  = Question -- front of the card
  | Answer -- back of the card

type alias Card =
  { word: Word
  , form: Form
  , input: String
  , state: State
  , timer: Timer.Timer
  }

exampleCard : Card
exampleCard =
  { word = exampleWord
  , form = exampleForm
  , input = ""
  , state = Question
  , timer = Timer.off
  }

randomCard : Settings -> Random.Generator Card
randomCard settings =
  let
    buildCard pair =
      let
        (word, form) = pair
      in
        { word = word
        , form = form
        , input = ""
        , state = Question
        , timer = Timer.reset settings.questionTimeout
        }
  in
    Random.pair randomWord randomForm |> Random.map buildCard

correctAnswer : Card -> Result String Word
correctAnswer card =
  conjugate card.form card.word

isCorrect : Settings -> Card -> Bool
isCorrect settings card =
  case correctAnswer card of
    Err err -> False
    Ok answer ->
      card.input == wordToString settings.inputFormat answer

wordToString : InputFormat -> Word -> String
wordToString inputFormat word =
  case inputFormat of
    RomajiInput -> toRomaji word
    KanaInput -> toKana word
    KanjiInput -> toKanji word

-- UPDATE

type Msg
  = SetInput String
  | SubmitInput
  | TimerMsg Timer.Msg
  | NextCard Card


update : Msg -> Settings -> Card -> (Card, Cmd Msg)
update msg settings card =
  case msg of
    SetInput input ->
      ({ card | input = input }, Cmd.none)

    SubmitInput ->
      case card.state of
        Question ->
          (validateCurrentCard settings card, Cmd.none)
        Answer ->
          if isCorrect settings card
          then (card, generateNextCard settings)
          else (card, Cmd.none)

    NextCard newCard ->
      (newCard, Cmd.none)

    TimerMsg msg ->
      let
        timer = Timer.update msg card.timer
        card' = { card | timer = timer }
      in
        if Timer.hasRunOutOfTime timer
        then
          case card.state of
            Question ->
              (validateCurrentCard settings card, Cmd.none)
            Answer ->
              (card', generateNextCard settings)
        else
          (card', Cmd.none)

validateCurrentCard : Settings -> Card -> Card
validateCurrentCard settings card =
  { card | state = Answer, timer = Timer.reset settings.answerTimeout }

generateNextCard : Settings -> Cmd Msg
generateNextCard settings =
  Random.generate NextCard (randomCard settings)

-- VIEW

view : Settings -> Card -> Html Msg
view settings card =
  div []
    [ Timer.view card.timer
    , viewQuestion settings card
    , viewInput settings card
    , if card.state == Answer && (not <| isCorrect settings card)
      then viewAnswer settings card
      else text ""
    ]

viewQuestion : Settings -> Card -> Html Msg
viewQuestion settings card =
  div [ class "card-row" ]
    [ div [ class "card-word" ]
        [ span [] [ text "Word: " ]
        , wordToHtml settings.outputFormat card.word
        ]
    , div [ class "card-form" ]
        [ span [] [ text "Form: " ]
        , formToHtml card.form
        ]
    ]

wordToHtml : OutputFormat -> Word -> Html Msg
wordToHtml outputFormat word =
  case outputFormat of
    RomajiOutput -> text <| toRomaji word
    KanaOutput -> text <| toKana word
    FuriganaOutput -> toRuby word
    KanjiOutput -> text <| toKanji word

formToHtml : Form -> Html Msg
formToHtml form =
  text <| formDescription form

viewInput : Settings -> Card -> Html Msg
viewInput settings card =
  let
    showValid =
      card.state == Answer && isCorrect settings card
    showInvalid =
      card.state == Answer && (not <| isCorrect settings card)
  in
    form
      [ class "card-row"
      , onSubmit SubmitInput
      ]
      [ div [ class "card-input" ]
          [ input
              [ classList
                  [ ("valid", showValid)
                  , ("invalid", showInvalid)
                  ]
              , value card.input
              , onInput SetInput
              ]
              []
          ]
      ]

viewAnswer : Settings -> Card -> Html Msg
viewAnswer settings card =
  div [ class "card-row" ]
    [ case correctAnswer card of
        Ok conjugated ->
          div [ class "card-answer" ]
            [ input
                [ readonly True
                , value <| wordToString settings.inputFormat conjugated
                ]
                []
            ]
        Err err ->
          p [ class "text-danger" ] [ text err ]
    ]

-- SUBSCRIPTIONS

subscriptions : Card -> Sub Msg
subscriptions card =
  Sub.map TimerMsg <| Timer.subscriptions card.timer
