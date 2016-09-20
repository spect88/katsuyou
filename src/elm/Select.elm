module Select exposing
  (selectMenu)

import Html exposing (Html, select, option, text)
import Html.Attributes exposing (id, value, selected, class)
import Html.Events exposing (on)
import Json.Decode as Json
import Dict

selectMenu : String -> a -> List {value: a, label: String} -> Html a
selectMenu id' currentValue options =
  let
    buildOption v label' =
      option
        [ value (toString v)
        , selected (currentValue == v) ]
        [ text label' ]

    valueLookup =
      options
        |> List.map (\{value} -> (toString value, value))
        |> Dict.fromList

    valueDecoder value =
      case Dict.get value valueLookup of
        Just m -> Json.succeed m
        Nothing -> Json.fail ("Unknown select value: " ++ value)

    changeHandler =
      Json.at ["target", "value"] Json.string `Json.andThen` valueDecoder

    optionsHtml =
      List.map (\{value, label} -> buildOption value label) options
  in
    select
      [ id id'
      , class "form-control"
      , on "change" changeHandler
      ]
      optionsHtml
