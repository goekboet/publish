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
        [ h3 [] [ text "Search times."]
        , p [] [ text "Step through your published times past or coming weeks or go back to current week."]
        , span  [ class "weekstepper" ] 
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
-- type alias Week =
--     String

-- type alias Start =
--     Int

-- type alias End =
--     Int
-- type alias Window = ( Start, End )

-- type alias WeekPointer =
--     { prev : Week
--     , curr : Week
--     , next : Week
--     , window : Window
--     }

-- encodeWeek : Week -> Value
-- encodeWeek w =
--     Encode.string w

-- decodeTimesWindow : Decoder Window
-- decodeTimesWindow =
--     Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)


-- decodeWeekpointer : Decoder WeekPointer
-- decodeWeekpointer =
--     Decode.map4
--         WeekPointer
--         (Decode.field "prev" Decode.string)
--         (Decode.field "curr" Decode.string)
--         (Decode.field "next" Decode.string)
--         (Decode.field "window" decodeTimesWindow)

-- addWeekFocusQuery : Route -> Week -> String
-- addWeekFocusQuery r w =
--     let
--         query =
--             UrlB.string "week" w
--     in
--     case r of
--         PublishRoute h _ ->
--             UrlB.absolute [ "publish", h ] [ query ]

--         _ ->
--             UrlB.absolute [] [ query ]

-- weekpointerStaleness : Route -> WeekPointer -> Maybe Week
-- weekpointerStaleness r wp =
--     case r of
--         PublishRoute _ (Just wq) ->
--             if wq == wp.curr then
--                 Nothing

--             else
--                 Just wq

--         _ ->
--             Nothing

-- refreshStaleWeekpointer : Route -> WeekPointer -> Cmd msg
-- refreshStaleWeekpointer r wp =
--     case weekpointerStaleness r wp of
--         Just w ->
--             getWeekpointer <| encodeWeek w

--         _ ->
--             Cmd.none

-- weekPointerView : Route -> Maybe WeekPointer -> Html msg
-- weekPointerView r wp =
--     case wp of
--         Just { prev, curr, next, window } ->
--             let
--                 ( y, w ) =
--                     case String.split "-" curr of
--                         [ year, week ] ->
--                             ( year, week )

--                         _ ->
--                             ( "", "" )
--             in
--                 div [ class "weekpointer" ]
--                 [ p [ id "yearlabel", class "label"] [ text "Year" ]
--                 , p [ id "weeklabel", class "label"] [ text "Week" ]
--                 , a [ id "wp", class "prev", href <| addWeekFocusQuery r prev ] [ text "◀" ]
--                 , p [ id "yearvalue", class "value" ] [ text y ]
--                 , p [ id "weekvalue", class "value" ] [ text w ]
--                 , a [ id "wn", class "next", href <| addWeekFocusQuery r next ] [ text "▶" ]
--                 , p [ id "weekdaylabel", class "label" ] [ text "weekday"]
--                 , a [ id "dp", class "prev" ] [ text "◀"]
--                 , p [ id "weekdayvalue", class "value" ] [ text "someday"]
--                 , a [ id "dn", class "next" ] [ text "▶"] 
--                 ]
    
--         Nothing -> div 
--             []
--             [ h2 [] [ text "Error"]
--             , p [] 
--                 [ text "There was an error. We cannot display a week-picker here. You could try to "
--                 , a [ href "/" ] [ text "reload" ]
--                 , text " publish."
--                 ]
--             ]
            
    
    

