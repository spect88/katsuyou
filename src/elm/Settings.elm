module Settings exposing
  ( Settings
  , Msg
  , OutputFormat(..)
  , InputFormat(..)
  , defaultSettings
  , update
  , view
  )

import Html exposing (Html, div, h2, text, select, option, label)
import Html.Attributes exposing (for, class)
import Html.App as Html

import Select exposing (selectMenu)

-- MODEL

type InputFormat
  = RomajiInput
  | KanaInput
  | KanjiInput

type OutputFormat
  = RomajiOutput
  | KanaOutput
  | FuriganaOutput
  | KanjiOutput

type alias Seconds = Int

type alias Settings =
  { inputFormat: InputFormat
  , outputFormat: OutputFormat
  , questionTimeout: Seconds
  , answerTimeout: Seconds
  }

defaultSettings : Settings
defaultSettings =
  { inputFormat = RomajiInput
  , outputFormat = FuriganaOutput
  , questionTimeout = 30
  , answerTimeout = 10
  }

-- UPDATE

type Msg
  = SetInputFormat InputFormat
  | SetOutputFormat OutputFormat

update : Msg -> Settings -> Settings
update msg settings =
  case msg of
    SetInputFormat format ->
      { settings | inputFormat = format }
    SetOutputFormat format ->
      { settings | outputFormat = format }

-- VIEW

view : Settings -> Html Msg
view settings =
  div []
    [ div [ class "settings-row" ]
        [ inputFormatMenu settings
        , outputFormatMenu settings
        ]
    ]

inputFormatMenu : Settings -> Html Msg
inputFormatMenu settings =
  div [ class "settings-item" ]
    [ label [ for "input_format" ] [ text "Japanese input format" ]
    , selectMenu "input_format" settings.inputFormat
        [ { value = RomajiInput, label = "Romaji" }
        , { value = KanaInput, label = "Kana" }
        , { value = KanjiInput, label = "Kanji" }
        ]
        |> Html.map (\format -> SetInputFormat format)
    ]

outputFormatMenu : Settings -> Html Msg
outputFormatMenu settings =
  div [ class "settings-item" ]
    [ label [ for "output_format" ] [ text "Japanese output format" ]
    , selectMenu "output_format" settings.outputFormat
        [ { value = RomajiOutput, label = "Romaji" }
        , { value = KanaOutput, label = "Kana" }
        , { value = FuriganaOutput, label = "Furigana" }
        , { value = KanjiOutput, label = "Kanji" }
        ]
        |> Html.map (\format -> SetOutputFormat format)
    ]
