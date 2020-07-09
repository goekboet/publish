module Weekpointer exposing 
    ( Window
    , Model
    , setDay
    , view
    )

import Html exposing (Html, div, text, span, button, h3)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)

type alias Window = (Int, Int)

type alias Model =
    { name : String
    , window : Window
    , day : String
    }

setDay : Model -> String -> Model
setDay wp d = 
    { wp | day = d }

view : (String -> msg) -> msg -> msg -> msg -> Model -> Html msg
view changeday pw cw nw wp =
    let
       dayfocus d = classList 
        [ ("heavy", d == wp.day) 
        , ("light", d /= wp.day)
        ] 
    in
    div 
        [] 
        [ span [ class "weekstepper" ] 
          [ h3 [] [ text wp.name ]
          , button [ onClick pw ] [ text "previous" ]
          , button [ onClick cw ] [ text "current" ]
          , button [ onClick nw ] [ text "next" ]
          ]
        , span [ class "daystepper" ] 
          [ button [ dayfocus "Mon", onClick (changeday "Mon" ) ] [ text "mon" ]
          , button [ dayfocus "Tue", onClick (changeday "Tue" ) ] [ text "tue" ] 
          , button [ dayfocus "Wed", onClick (changeday "Wed" ) ] [ text "wed" ] 
          , button [ dayfocus "Thu", onClick (changeday "Thu" ) ] [ text "thu" ] 
          , button [ dayfocus "Fri", onClick (changeday "Fri" ) ] [ text "fri" ] 
          , button [ dayfocus "Sat", onClick (changeday "Sat" ) ] [ text "sat" ] 
          , button [ dayfocus "Sun", onClick (changeday "Sun" ) ] [ text "sun" ] 
          ]
        ]