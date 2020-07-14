port module Publish exposing 
    ( TsLookup
    , Model
    , init
    , listTimes
    , view
    , Msg
    , update
    , subscribe
    )

import Html exposing (Html)
import Html.Attributes as Attr exposing (classList, type_, readonly, placeholder, size)
import Http  
import Html.Events as Event
import FontAwesome as FA
import Json.Decode as Json
import Json.Encode as Encode exposing (Value)
import Url.Builder as Url

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
    , start: Int
    , end : Int
    , isNow : Bool
    , hours: List Hour
    }

type alias Weekpointer =
    { current: Week
    , previous: Week
    , next : Week
    }

getWeekWindow : Weekpointer -> (Int, Int)
getWeekWindow { current, previous, next } =
    (current.ts, next.ts)

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

getConflicts : Timepointer -> TimepublishData -> (List String)
getConflicts tp (_, ts) =
    let
        staged = toStagedTime tp
            
        conflict { start, end, name } t 
          = start <= t.end && end >= t.start   
    in
        case staged of
        Just t ->
            ts
            |> List.map Tuple.second
            |> List.filter (conflict t)
            |> List.sortBy .start
            |> List.map .name
        _      -> [] 


type alias Model =
    { weekpointer : Weekpointer
    , daypointer : Daypointer
    , timepointer : Timepointer
    , data: TimepublishData
    }

init : TsLookup -> Bool -> Model
init ts fetching =
    let
        dptr = initDaypointer ts
        tptr = initTimePointer dptr
    in
    { weekpointer = ts.week
    , daypointer = dptr
    , timepointer = tptr
    , data = if fetching then (Pending, []) else (Submitted, [])
    }

recalculate : Model -> TsLookup -> Model
recalculate { weekpointer, daypointer, timepointer, data } ts =
    let
       newDptr = { daypointer | days = ts.days } 
       newTptr = { timepointer | hours = getSelectedHours newDptr }
    in
    { weekpointer = ts.week 
    , daypointer = newDptr
    , timepointer = newTptr
    , data = registerListTimesPending data
    }

selectDay : Model -> Int -> Model
selectDay { weekpointer, daypointer, timepointer, data } d =
    let
        newDptr = { daypointer | selectedDay = d }
        newTptr = { timepointer | hours = getSelectedHours newDptr }
    in
    { weekpointer = weekpointer 
    , daypointer = newDptr
    , timepointer = newTptr
    , data = data
    } 

selectHour : Model -> Int -> Model
selectHour { weekpointer, daypointer, timepointer, data } h =
    let
        newTptr = { timepointer | selected = modBy (List.length timepointer.hours) h}
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    , data = data
    }

selectMinute : Model -> Int -> Model
selectMinute { weekpointer, daypointer, timepointer, data } m =
    let
        newTptr = { timepointer | minutes = modBy 60 m}
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    , data = data
    }

selectDuration : Model -> Int -> Model
selectDuration { weekpointer, daypointer, timepointer, data } d =
    let
        newTptr = { timepointer | duration = d }
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    , data = data
    }

selectPause : Model -> Int -> Model
selectPause { weekpointer, daypointer, timepointer, data } p =
    let
        newTptr = { timepointer | pause = p }
    in
    { weekpointer = weekpointer 
    , daypointer = daypointer
    , timepointer = newTptr
    , data = data
    }

registerPending : TimepublishData -> StagedTime -> TimepublishData
registerPending (status, data) t = (status, (Pending, t) :: data)

updateStatus : TimepublishData -> Int -> RequestStatus -> TimepublishData
updateStatus (status, data) id newStatus =
    let
        f (ts, t) = if t.start == id then (newStatus, t) else (ts, t)
    in
        (status, List.map f data)

removeTime : TimepublishData -> Int -> TimepublishData   
removeTime (status, ts) id =
    let
        newTs = 
          ts
          |> List.filter (\(s, t) -> t.start /= id)
    in
        (status, newTs)
    

setSubmitted : Model -> Int -> Model
setSubmitted m id = { m | data = updateStatus m.data id Submitted }

setError : Model -> Int -> Model
setError m id = { m | data = updateStatus m.data id Errored }

setPending : Model -> Int -> Model
setPending m id = { m | data = updateStatus m.data id Pending }

advance : Model -> Bool -> (Int -> (Result Http.Error StagedTime) -> msg) -> (Model, Cmd msg)
advance { weekpointer, daypointer, timepointer, data } submit appMsg =
    let
        newSelected = timepointer.selected + (timepointer.duration + timepointer.pause) // 60
        newMinutes = remainderBy 60 (timepointer.duration + timepointer.pause)
        newTptr = { timepointer | selected = newSelected, minutes = newMinutes }
        submission = toStagedTime timepointer
        submitCommand =
            if submit
            then
                submission
                |> Maybe.map (submitTime appMsg)
                |> Maybe.withDefault Cmd.none
            else 
                Cmd.none

        newData =
            if submit
            then 
                submission
                |> Maybe.map (registerPending data)
                |> Maybe.withDefault data
            else
                data
    in
    ( { weekpointer = weekpointer 
      , daypointer = daypointer
      , timepointer = newTptr
      , data = newData
      }
    , submitCommand
    )

type alias TimePublish =
  { start : Int
  , name : String
  , end : Int
  }

fromTimePayload : Json.Decoder TimePublish
fromTimePayload =
  Json.map3 TimePublish
    (Json.field "start" Json.int)
    (Json.field "name" Json.string)
    (Json.field "end" Json.int)

toTimePayload : TimePublish -> Value
toTimePayload { start, name, end } =
  Encode.object
    [ ("Start", Encode.int start)
    , ("Name", Encode.string name)
    , ("End", Encode.int end)
    ]

type RequestStatus 
    = Submitted
    | Pending 
    | Errored 

type alias TimepublishSubmission = (RequestStatus, TimePublish)

type alias TimepublishData = (RequestStatus, (List TimepublishSubmission))

refreshTimes : TimepublishData -> (List StagedTime) -> TimepublishData
refreshTimes (status, oldData) ts =
    let
        keep s =
            case s of
            Submitted -> False
            _         -> True

        clientState = oldData
            |> List.filter (keep << Tuple.first)
    in
        (Submitted, clientState ++ (ts |> List.map (Tuple.pair Submitted)))

registerListTimesError : TimepublishData -> TimepublishData
registerListTimesError (status, ts) = (Errored, ts)

registerListTimesPending : TimepublishData -> TimepublishData
registerListTimesPending (status, ts) = (Pending, ts)

listTimes : (Msg -> msg) -> Weekpointer -> Cmd msg
listTimes toAppMsg wp = 
    let
        toQ (f, t) = 
          [ Url.int "from" f
          , Url.int "to" t
          ]
        q = 
          getWeekWindow wp
          |> toQ

    in
    Http.get
    { url = Url.absolute [ "api", "times"] q
    , expect = Http.expectJson (toAppMsg << GotTimes) (Json.list fromTimePayload)
    }

submitTime : (Int -> (Result Http.Error StagedTime) -> msg) -> StagedTime -> Cmd msg
submitTime response t  =
    Http.post
          { url = Url.absolute [ "api", "times"] []
          , body = Http.jsonBody (toTimePayload t)
          , expect = Http.expectJson (response t.start) fromTimePayload
          }

unpublishTime : Int -> (Result Http.Error () -> msg) -> Cmd msg
unpublishTime id toAppMsg =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = Url.absolute [ "api", "times", String.fromInt id] []
    , body = Http.emptyBody
    , expect = Http.expectWhatever toAppMsg 
    , timeout = Nothing
    , tracker = Nothing
    }

type Msg 
    = Move (Maybe Int)
    | New TsLookup
    | SelectDay Int
    | SelectHour Int
    | SelectMinute Int
    | SelectDuration Int
    | SelectPause Int
    | Advance Bool
    | TimeSubmitted Int (Result Http.Error StagedTime)
    | GotTimes (Result Http.Error (List StagedTime))
    | RefreshTimes
    | Unpublish Int
    | Unpublished Int (Result Http.Error ())
    | DismissError Int

port moveWeekpointer : (Maybe Int) -> Cmd msg
port newWeekpointer : (TsLookup -> msg) -> Sub msg

subscribe : (Msg -> msg) -> Sub msg 
subscribe toAppmsg =
    newWeekpointer (toAppmsg << New)

update : Model -> Msg -> (Msg -> msg) -> (Model, Cmd msg)
update m cmd toAppMsg =
    case cmd of
    Move ts -> 
        ( m, moveWeekpointer ts )

    New lookup -> 
        let
            newModel = recalculate m lookup     
        in
        ( newModel 
        , listTimes toAppMsg newModel.weekpointer
        )

    SelectDay d ->
        ( selectDay m d
        , Cmd.none
        )

    SelectHour h ->
        ( selectHour m h, Cmd.none )

    SelectMinute min ->
        ( selectMinute m min, Cmd.none )

    SelectDuration dur ->
        ( selectDuration m dur, Cmd.none )

    SelectPause p ->
        ( selectPause m p, Cmd.none )

    Advance submit -> advance m submit (\id t -> TimeSubmitted id t |> toAppMsg)

    TimeSubmitted id (Ok _) ->
        ( setSubmitted m id, Cmd.none)

    TimeSubmitted id (Err _) ->
        ( setError m id, Cmd.none)

    GotTimes (Ok ts) ->
        ( { m | data = refreshTimes m.data ts }, Cmd.none)

    GotTimes (Err _) ->
        ( { m | data = registerListTimesError m.data }, Cmd.none)

    RefreshTimes ->
        ( { m | data = registerListTimesPending m.data }
        , listTimes toAppMsg m.weekpointer
        )

    Unpublish id ->
        ( setPending m id, unpublishTime id (toAppMsg << Unpublished id) )

    Unpublished id (Ok _) ->
        ( { m | data = removeTime m.data id }, Cmd.none)

    Unpublished id (Err _) ->
        ( setError m id, Cmd.none)

    DismissError id ->
        ( { m | data = removeTime m.data id }, Cmd.none)

weekPointerControls : (Maybe Int -> msg) -> Weekpointer -> List (Html msg) 
weekPointerControls toMsg { current, previous, next } =
    [ Html.h4 [] [ Html.text "Switch to current, previous or next week." ]
    , Html.span []
      [ Html.button 
        [ Event.onClick (Nothing |> toMsg) ] 
        [ FA.fas_fa_chevron_circle_down ]
      , Html.button 
        [ Event.onClick (previous.ts |> Just |> toMsg) ] 
        [ FA.fas_fa_arrow_alt_circle_left ]
      , Html.button 
        [ Event.onClick (next.ts |> Just |> toMsg) ] 
        [ FA.fas_fa_arrow_alt_circle_right ]
      , Html.label [] [ Html.text current.name ]
      ]
    ]

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
    [ Html.h4 [] [ Html.text "Point out the weekday." ]
    , Html.span []
      ( List.indexedMap Tuple.pair days
        |> List.map dayBtn )
    ]

timepointerControl : (Int -> msg) -> (Int -> msg) -> (Int -> msg) -> (Int -> msg) -> Timepointer -> List (Html msg)
timepointerControl selectHourMsg selectMinuteMsg selectDurationMsg selectPauseMsg { hours, selected, minutes, duration, pause } =
    let
        h = hours
            |> List.map .name
            |> List.drop selected
            |> List.head
            |> Maybe.withDefault "00"            
    in
    [ Html.h4 [] [ Html.text "Stage a time" ]
    , Html.span []
      [ Html.span [] 
        [ "Time:" |> Html.text |> List.singleton |> Html.label []
        , Html.input 
          [ type_ "text"
          , readonly True
          , placeholder h 
          , size 2
          , arrowKeyIncrement selected 1 selectHourMsg
          ] 
          []
        , ":" |> Html.text |> List.singleton |> Html.label []
        , Html.input 
          [ type_ "text"
          , readonly True
          , minutes |> String.fromInt |> String.pad 2 '0' |> placeholder 
          , size 2
          , arrowKeyIncrement minutes 5 selectMinuteMsg
          ] 
        []
        ]
      , Html.span []
        [ "duration:" |> Html.text |> List.singleton |> Html.label []
        , Html.input 
          [ type_ "text"
          , readonly True
          , duration |> String.fromInt |> placeholder 
          , size 2
          , arrowKeyIncrement duration 5 selectDurationMsg
          ] 
          []
        ]
      , Html.span []
        [ "pause:" |> Html.text |> List.singleton |> Html.label []
        , Html.input 
          [ type_ "text"
          , readonly True
          , pause |> String.fromInt |> placeholder 
          , size 2
          , arrowKeyIncrement pause 5 selectPauseMsg
          ] 
          []
        ]
      ]
    ]
  

stagedTimeControl : (Bool -> msg) ->  Timepointer -> TimepublishData -> List (Html msg)
stagedTimeControl adv timepointer data =
    let
        conflicts = getConflicts timepointer data
        isOk = List.isEmpty conflicts
    in
    case toStagedTime timepointer of
    Just t -> 
        [ Html.h4 [] [ Html.text "Review staged time"]
        , Html.span []
          [ Html.span []  
            [ FA.far_fa_check_circle 
            , Html.label [] [ Html.text t.name ]
            , Html.button 
              [ adv False |> Event.onClick ]
              [ Html.text "skip" ]
            , Html.button 
              ( if isOk then [ adv True |> Event.onClick ] else [ Attr.disabled True ] )
              [ Html.text "publish" ]
            ]
          , Html.span []
            [ Html.i [] [ Html.text "Conflicting times:" ]
            ,  Html.b [] [ conflicts |>  String.join ", " |> Html.text ] 
            ]
          ]
        ]
    _                         -> []

timePayloadView : (Msg -> msg) -> TimepublishSubmission -> List (Html msg)
timePayloadView toAppMsg (status, t) = 
    case status of
    Submitted -> 
        [ FA.fas_fa_check_circle 
        , Html.label [] [ Html.text t.name ]
        , Html.button 
          [ Event.onClick (Unpublish t.start |> toAppMsg)] 
          [ Html.text "unpublish" ]
        ]
    Pending ->   
        [ FA.fas_fa_sync_alt_rolls
        , Html.label [] [ Html.text t.name ] 
        ]
    Errored ->  
        [ FA.fas_fa_exclamation_triangle
        , Html.label [] [ Html.text ("Submission failed for " ++ t.name) ]
        , Html.button 
          [ Event.onClick (DismissError t.start |> toAppMsg)] 
          [ Html.text "dismiss" ]
        ]

timepublishDataView : (Msg -> msg) -> TimepublishData -> Daypointer -> List (Html msg)
timepublishDataView toAppMsg (status, data) dp =
    let
        day = getSelectedDay dp
        f t d = t.start >= d.start && t.start <= d.end  
        ts = 
          data
          |> List.filter (\(s, t) -> Maybe.map (f t) day |> Maybe.withDefault False ) 
          |> List.sortBy (.start << Tuple.second)   
    in
    case status of
    Submitted -> 
        [ Html.span [] 
          [ Html.button 
            [ Event.onClick (toAppMsg RefreshTimes) ] 
            [ FA.fas_fa_sync_alt ]
          , Html.h3 [] [ Html.text "Published times: " ]
          ]
        , List.map (Html.li [] << timePayloadView toAppMsg) ts |> Html.ul []  ]
    Pending -> 
        [ Html.span [] 
          [ FA.fas_fa_sync_alt_rolls
          , Html.h3 [] [ Html.text "Published times:" ]
          ]
        , List.map (Html.li [] << timePayloadView toAppMsg) ts |> Html.ul []  ]
    Errored -> 
        [ Html.span [ ] 
            [ Html.button 
              [ Event.onClick (toAppMsg RefreshTimes) ] 
              [ FA.fas_fa_exclamation_triangle ]
            , Html.h3 [] [ Html.text "Error fetching times."]
            ]
        , List.map (Html.li [] << timePayloadView toAppMsg) ts |> Html.ul []  ]

view : (Msg -> msg) -> Model -> List (Html msg)
view toAppMsg m = 
    [ Html.h2 [] [ Html.text "Publish a time" ]
    , weekPointerControls (toAppMsg << Move) m.weekpointer
      |> Html.span [ Attr.class "weekpointer" ] 
    , selectDayControl (toAppMsg << SelectDay) m.daypointer
      |> Html.span [ Attr.class "daypointer" ]
    , timepointerControl (toAppMsg << SelectHour) (toAppMsg << SelectMinute) (toAppMsg << SelectDuration) (toAppMsg << SelectPause) m.timepointer 
      |> Html.span [ Attr.class "timepointer" ]
    , stagedTimeControl (toAppMsg << Advance) m.timepointer m.data
      |> Html.span [ Attr.class "stagedTime" ]
    , timepublishDataView toAppMsg m.data m.daypointer
      |> Html.span [ Attr.class "timepublishData" ]
    ]
    