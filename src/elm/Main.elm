import Html exposing (Html, div, button, text)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Html.App as Html

import Settings
import Card
import Layout

main : Program Never
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- MODEL

type alias Model =
  { settings: Settings.Settings
  , card: Maybe Card.Card
  , settingsVisible: Bool
  }

init : (Model, Cmd Msg)
init =
  (model, Cmd.none)

model : Model
model =
  let
    settings = Settings.defaultSettings
  in
    { settings = settings
    , card = Nothing
    , settingsVisible = True
    }

-- UPDATE

type Msg
  = SettingsMsg Settings.Msg
  | CardMsg Card.Msg
  | Start

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SettingsMsg msg' ->
      ({ model | settings = Settings.update msg' model.settings }, Cmd.none)

    CardMsg msg' ->
      let
        card' = Maybe.withDefault Card.exampleCard model.card
        (card, cmd) = Card.update msg' model.settings card'
      in
        ({ model | card = Just card }, Cmd.map CardMsg cmd)

    Start ->
      let
        cmd = Card.generateNextCard model.settings
      in
        ({ model | settingsVisible = False }, Cmd.map CardMsg cmd)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.card of
    Nothing ->
      Sub.none

    Just card ->
      Sub.map CardMsg <| Card.subscriptions card

-- VIEW

view : Model -> Html Msg
view model =
  Layout.container
    [ Layout.header
    , if model.settingsVisible
      then Settings.view model.settings |> Html.map SettingsMsg
      else text ""
    , case model.card of
        Just card ->
          Card.view model.settings card |> Html.map CardMsg
        Nothing ->
          startButton
    , Layout.footer
    ]

startButton : Html Msg
startButton =
  div [ class "start-button" ]
    [ button
        [ onClick Start ]
        [ text "Start" ]
    ]
