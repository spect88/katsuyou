module Util exposing (sample)

import Random
import Debug exposing (crash)

sample : List a -> Random.Generator a
sample list =
  let
    pickElement i =
      case list |> List.drop i |> List.head of
        Just elem -> elem
        Nothing -> crash "this should never happen ;)"
  in
    if List.isEmpty list
    then crash "trying to take a random element from empty list"
    else Random.map pickElement <| Random.int 0 <| List.length list - 1
