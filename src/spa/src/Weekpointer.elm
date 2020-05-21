port module Weekpointer exposing (Weekpointer, initWeekpointer, weekpointerView)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, field, string)
import Route exposing (Route(..))
import Url.Builder as UrlB
import Html exposing (Html, div, text, a, p, h2, span, button, h3 )
import Html.Attributes exposing (class, classList, id, href)
import Html.Events exposing (onClick)

port getWeekpointer : Value -> Cmd a
port gotWeekpointer : (Value -> msg) -> Sub msg

type alias Weekpointer =
    { name : String 
    , day : String
    }

initWeekpointer : Weekpointer
initWeekpointer = { name = "mon, w45 2020", day = "mon" }

weekpointerView : (String -> msg) -> Weekpointer -> Html msg
weekpointerView changeday wp =
    let
       dayfocus d = classList 
        [ ("heavy", d == wp.day) 
        , ("light", d /= wp.day)
        ] 
    in
    div 
        [] 
        [ span  [ class "weekstepper" ] 
            [ h3 [] [text wp.name] 
            , button [] [ text "previous" ]
            , button [] [ text "current" ]
            , button [] [ text "next" ]
            ]
        , span [ class "daystepper" ] 
            [ button [ dayfocus "mon", onClick (changeday "mon" ) ] [ text "mon" ]
            , button [ dayfocus "tue", onClick (changeday "tue" ) ] [ text "tue" ] 
            , button [ dayfocus "wed", onClick (changeday "wed" ) ] [ text "wed" ] 
            , button [ dayfocus "thu", onClick (changeday "thu" ) ] [ text "thu" ] 
            , button [ dayfocus "fri", onClick (changeday "fri" ) ] [ text "fri" ] 
            , button [ dayfocus "sat", onClick (changeday "sat" ) ] [ text "sat" ] 
            , button [ dayfocus "sun", onClick (changeday "sun" ) ] [ text "sun" ] 
            ]
        ]