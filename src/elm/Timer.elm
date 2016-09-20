module Timer exposing
  ( Timer
  , Msg(..)
  , hasRunOutOfTime
  , reset
  , off
  , update
  , view
  , subscriptions
  )

import Time exposing (Time, second)

import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

-- MODEL

type alias Seconds = Int
type alias Timer =
  { on: Bool, elapsed: Seconds, total: Seconds }

reset : Seconds -> Timer
reset totalTime =
  { on = True, elapsed = 0, total = totalTime }

off : Timer
off =
  { on = False, elapsed = 0, total = 0 }

hasRunOutOfTime : Timer -> Bool
hasRunOutOfTime timer =
  timer.on && timer.elapsed >= timer.total

isOn : Timer -> Bool
isOn timer =
  timer.on

-- UPDATE

type Msg = Tick Time

update : Msg -> Timer -> Timer
update msg timer =
  case msg of
    Tick _ ->
      if isOn timer
      then { timer | elapsed = timer.elapsed + 1 }
      else timer

-- VIEW

countdown : Timer -> Seconds
countdown timer =
  timer.total - timer.elapsed

view : Timer -> Html msg
view timer =
  if isOn timer
  then
    div [ class "timer" ] [ text <| toString <| countdown timer ]
  else
    text ""

-- SUBSCRIPTIONS

subscriptions : Timer -> Sub Msg
subscriptions timer =
  if isOn timer
  then Time.every second Tick
  else Sub.none
