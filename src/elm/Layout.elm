module Layout exposing
  ( container
  , header
  , footer
  )

import Html exposing (Html, h1, ruby, span, rp, rt, text, div, a)
import Html.Attributes exposing (class, href)

-- VIEW

container content =
  div [ class "layout-container" ] content

header =
  h1 [ class "layout-header" ]
    [ ruby []
        [ span [] [ text "活用" ]
        , rp [] [ text "(" ]
        , rt [] [ text "katsuyou" ]
        , rp [] [ text ")" ]
        ]
    ]

footer =
  div [ class "layout-footer" ]
    [ text "Built with "
    , a [ href "http://elm-lang.org" ]
        [ text "Elm" ]
    , text ". "
    , a [ href "https://github.com/spect88/katsuyou" ]
        [ text "Fork me on GitHub." ]
    ]
