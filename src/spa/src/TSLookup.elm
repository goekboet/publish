module TSLookup exposing 
    ( TsLookup
    , Model
    , StagedTime
    , init
    , weekPointerControls
    , recalculate
    , selectDay
    , selectDayControl
    , selectHour
    , selectMinute
    , selectDuration
    , selectPause
    , timepointerControl
    , stagedTimeControl
    , advance
    )

import Html exposing (Html)
import Html.Events as Event
import Html.Attributes exposing (classList, type_, readonly, placeholder, size)
import FontAwesome as FA
import Json.Decode as Json

type alias Week = 
    { name : String
    , ts: Int
    , isNow : Bool
    }

type alias Hour =
    { name : String
    , ts: Int
    , isNow : Bool
    }

type alias Day = 
    { key: String
    , name: String
    , ts: Int
    , isNow : Bool
    , hours: List Hour
    }

type alias Weekpointer =
    { current: Week
    , previous: Week
    , next : Week
    }

weekPointerControls : (Maybe Int -> msg) -> Weekpointer -> List (Html msg) 
weekPointerControls toMsg { current, previous, next } =
    [ Html.h3 [] [ Html.text current.name ]
    , Html.button 
      [ Event.onClick (Nothing |> toMsg) ] 
      [ FA.fas_fa_chevron_circle_down ]
    , Html.button 
      [ Event.onClick (previous.ts |> Just |> toMsg) ] 
      [ FA.fas_fa_arrow_alt_circle_left ]
    , Html.button 
      [ Event.onClick (next.ts |> Just |> toMsg) ] 
      [ FA.fas_fa_arrow_alt_circle_right ]
    ]

type alias TsLookup =
    { week : Weekpointer
    , days : List Day
    }

type alias Daypointer = 
    { days: List Day
    , selectedDay : Int
    }

initDaypointer : TsLookup -> Daypointer
initDaypointer ts =
    let
        today = ts.days
            |> List.indexedMap Tuple.pair
            |> List.filter (.isNow << Tuple.second)
            |> List.map Tuple.first
            |> List.head
            |> Maybe.withDefault 0
    in
        { days = ts.days, selectedDay = today }

getSelectedDay : Daypointer -> Maybe Day
getSelectedDay dptr =
    dptr.days
    |> List.drop dptr.selectedDay
    |> List.head

getSelectedHours : Daypointer -> List Hour
getSelectedHours dptr =
    getSelectedDay dptr
    |> Maybe.map .hours
    |> Maybe.withDefault []

selectDayControl : (Int -> msg) -> Daypointer -> List (Html msg)
selectDayControl toAppMsg { days, selectedDay } =
    let
        dayBtn (i, d) = 
            Html.button 
            [ Event.onClick (toAppMsg i)
            , classList [ ("selectedDay", selectedDay == i) ]
            ] 
            [ Html.text d.key ]    
    in
    List.indexedMap Tuple.pair days
    |> List.map dayBtn


    

type alias Timepointer =
    { hours : List Hour
    , selected : Int
    , minutes : Int
    , duration : Int
    , pause : Int
    }

initTimePointer : Daypointer -> Timepointer
initTimePointer dptr =
    let
        hs = getSelectedHours dptr
        s = hs
            |> List.indexedMap Tuple.pair
            |> List.filter (.isNow << Tuple.second)
            |> List.map Tuple.first
            |> List.head
            |> Maybe.withDefault -1

    in 
        { hours = hs
        , selected = s
        , minutes = 0
        , duration = 45
        , pause = 15
        }

arrowKeyIncrement : Int -> Int -> (Int -> msg) -> Html.Attribute msg
arrowKeyIncrement base incr msg =
    let
        filter c = 
            case c of
            38 -> base + incr |> msg |> Json.succeed
            40 -> base - incr |> msg |> Json.succeed
            _  -> Json.fail ""
    in
        Event.on "keydown" (Json.andThen filter Event.keyCode)

timepointerControl : (Int -> msg) -> (Int -> msg) -> (Int -> msg) -> (Int -> msg) -> Timepointer -> List (Html msg)
timepointerControl selectHourMsg selectMinuteMsg selectDurationMsg selectPauseMsg { hours, selected, minutes, duration, pause } =
    let
        h = hours
            |> List.map .name
            |> List.drop selected
            |> List.head
            |> Maybe.withDefault "00"            
    in
    [ Html.text "Time:"
    , Html.input 
      [ type_ "text"
      , readonly True
      , placeholder h 
      , size 2
      , arrowKeyIncrement selected 1 selectHourMsg
      ] 
      []
    , Html.text ":"
    , Html.input 
      [ type_ "text"
      , readonly True
      , minutes |> String.fromInt |> String.pad 2 '0' |> placeholder 
      , size 2
      , arrowKeyIncrement minutes 5 selectMinuteMsg
      ] 
      []
    , Html.text "duration:"
    , Html.input 
      [ type_ "text"
      , readonly True
      , duration |> String.fromInt |> placeholder 
      , size 2
      , arrowKeyIncrement duration 5 selectDurationMsg
      ] 
      []
    , Html.text "pause:"
    , Html.input 
      [ type_ "text"
      , readonly True
      , pause |> String.fromInt |> placeholder 
      , size 2
      , arrowKeyIncrement pause 5 selectPauseMsg
      ] 
      []
    ]

type alias StagedTime =
    { start : Int
    , end : Int
    , name: String
    }

toStagedTime : Timepointer -> Maybe StagedTime
toStagedTime tp =
    let
        endH = tp.selected + (tp.minutes + tp.duration) // 60
        endM = remainderBy 60 (tp.minutes + tp.duration)
        hs = tp.hours
            |> List.drop tp.selected
            |> List.head

        he = tp.hours
            |> List.drop endH
            |> List.head

        f s e =
            { start = s.ts
            , end = e.ts + (endM * 60) 
            , name = s.name ++ ":" ++ (tp.minutes |> String.fromInt |> String.pad 2 '0') ++ " - " ++ e.name ++ ":" ++ (endM |> String.fromInt |> String.pad 2 '0') ++ " (" ++ (tp.duration |> String.fromInt) ++ " min)"
            }
    in
    Maybe.map2 f hs he 

stagedTimeControl : msg -> (StagedTime -> msg) ->  Timepointer -> List (Html msg)
stagedTimeControl skipMsg submitMsg tp =
    case toStagedTime tp of
    Just t -> 
        [ FA.far_fa_check_circle 
        , Html.p [] [ Html.text t.name ]
        , Html.button [ skipMsg |> Event.onClick] [ Html.text "skip" ]
        , Html.button [ submitMsg t |> Event.onClick ] [ Html.text "publish" ]
        ]
    _                         -> []


type alias Model =
    { weekpointer : Weekpointer
    , daypointer : Daypointer
    , timepointer : Timepointer
    }

init : TsLookup -> Model
init ts =
    let
        dptr = initDaypointer ts
        tptr = initTimePointer dptr
    in
    { weekpointer = ts.week
    , daypointer = dptr
    , timepointer = tptr
    }

recalculate : Model -> TsLookup -> Model
recalculate { weekpointer, daypointer, timepointer } ts =
    let
       newDptr = { daypointer | days = ts.days } 
       newTptr = { timepointer | hours = getSelectedHours newDptr }
    in
    { weekpointer = ts.week 
    , daypointer = newDptr
    , timepointer = newTptr
    }

selectDay : Model -> Int -> Model
selectDay { weekpointer, daypointer, timepointer } d =
    let
        newDptr = { daypointer | selectedDay = d }
        newTptr = { timepointer | hours = getSelectedHours newDptr }
    in
    { weekpointer = weekpointer 
    , daypointer = newDptr
    , timepointer = newTptr
    } 

selectHour : Model -> Int -> Model
selectHour { weekpointer, daypointer, timepointer } h =
    let
        newTptr = { timepointer | selected = modBy (List.length timepointer.hours) h}
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    }

selectMinute : Model -> Int -> Model
selectMinute { weekpointer, daypointer, timepointer } m =
    let
        newTptr = { timepointer | minutes = modBy 60 m}
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    }

selectDuration : Model -> Int -> Model
selectDuration { weekpointer, daypointer, timepointer } d =
    let
        newTptr = { timepointer | duration = d }
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    }

selectPause : Model -> Int -> Model
selectPause { weekpointer, daypointer, timepointer } p =
    let
        newTptr = { timepointer | pause = p }
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    }

advance : Model -> Model
advance { weekpointer, daypointer, timepointer } =
    let
        newSelected = timepointer.selected + (timepointer.duration + timepointer.pause) // 60
        newMinutes = remainderBy 60 (timepointer.duration + timepointer.pause)
        newTptr = { timepointer | selected = newSelected, minutes = newMinutes }
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    }
    




   
