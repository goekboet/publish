port module Publish exposing (Model, init, view, Msg, update, subscribe)

import Html exposing (Html)
import Html.Attributes as Attr
import TSLookup 
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import FontAwesome as FA

type alias Model = TSLookup.Model

init : TSLookup.TsLookup -> Model
init ts = TSLookup.init ts

type Msg 
    = Move (Maybe Int)
    | New TSLookup.TsLookup
    | SelectDay Int
    | SelectHour Int
    | SelectMinute Int
    | SelectDuration Int
    | SelectPause Int
    | Skip
    | Submit TSLookup.StagedTime

port moveWeekpointer : (Maybe Int) -> Cmd msg
port newWeekpointer : (TSLookup.TsLookup -> msg) -> Sub msg

subscribe : (Msg -> msg) -> Sub msg 
subscribe toAppmsg =
    newWeekpointer (toAppmsg << New)

update : Model -> Msg -> (Model, Cmd msg)
update m cmd =
    case cmd of
    Move ts -> 
        ( m, moveWeekpointer ts )

    New lookup -> 
        ( TSLookup.recalculate m lookup 
        , Cmd.none
        )

    SelectDay d ->
        ( TSLookup.selectDay m d
        , Cmd.none
        )

    SelectHour h ->
        ( TSLookup.selectHour m h, Cmd.none )

    SelectMinute min ->
        ( TSLookup.selectMinute m min, Cmd.none )

    SelectDuration dur ->
        ( TSLookup.selectDuration m dur, Cmd.none )

    SelectPause p ->
        ( TSLookup.selectPause m p, Cmd.none )

    Skip ->
        ( TSLookup.advance m, Cmd.none )

    Submit t ->
        ( TSLookup.advance m, Cmd.none )

type alias TimePublish =
  { start : Int
  , name : String
  , end : Int
  }

fromTimePayload : Decoder TimePublish
fromTimePayload =
  Decode.map3 TimePublish
    (Decode.field "start" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "end" Decode.int)

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

mockTimepublish : List TimepublishSubmission
mockTimepublish =
    [ ( Submitted, { start = 1594447200, name = "08:00 (45 min)", end = 1594449900 })
    , ( Submitted, { start = 1594450800, name = "09:00 (45 min)", end = 1594453500 })
    , ( Errored, { start = 1594454400, name = "10:00 (45 min)", end = 1594457100 })
    , ( Submitted, { start = 1594458000, name = "11:00 (45 min)", end = 1594460700 })
    , ( Submitted, { start = 1594461600, name = "12:00 (45 min)", end = 1594464300 })
    , ( Submitted, { start = 1594465200, name = "13:00 (45 min)", end = 1594467900 })
    , ( Submitted, { start = 1594468800, name = "14:00 (45 min)", end = 1594471500 })
    , ( Submitted, { start = 1594472400, name = "15:00 (45 min)", end = 1594475100 })
    , ( Submitted, { start = 1594476000, name = "16:00 (45 min)", end = 1594478700 })
    , ( Submitted, { start = 1594479600, name = "17:00 (45 min)", end = 1594482300 })
    , ( Submitted, { start = 1594483200, name = "18:00 (45 min)", end = 1594485900 })
    , ( Submitted, { start = 1594486800, name = "19:00 (45 min)", end = 1594489500 })
    , ( Submitted, { start = 1594490400, name = "20:00 (45 min)", end = 1594493100 })
    , ( Submitted, { start = 1594494000, name = "21:00 (45 min)", end = 1594496700 })
    , ( Submitted, { start = 1594497600, name = "22:00 (45 min)", end = 1594500300 })
    , ( Submitted, { start = 1594501200, name = "23:00 (45 min)", end = 1594503900 })
    , ( Submitted, { start = 1594504800, name = "00:00 (45 min)", end = 1594507500 })
    , ( Submitted, { start = 1594508400, name = "01:00 (45 min)", end = 1594511100 })
    , ( Submitted, { start = 1594512000, name = "02:00 (45 min)", end = 1594514700 })
    , ( Submitted, { start = 1594515600, name = "03:00 (45 min)", end = 1594518300 })
    , ( Submitted, { start = 1594519200, name = "04:00 (45 min)", end = 1594521900 })
    , ( Submitted, { start = 1594522800, name = "05:00 (45 min)", end = 1594525500 })
    , ( Submitted, { start = 1594526400, name = "06:00 (45 min)", end = 1594529100 })
    , ( Submitted, { start = 1594530000, name = "07:00 (45 min)", end = 1594532700 })
    , ( Submitted, { start = 1594533600, name = "08:00 (45 min)", end = 1594536300 })
    ]

timePayloadView : TimepublishSubmission -> List (Html msg)
timePayloadView (status, t) = 
    case status of
    Submitted -> 
        [ FA.fas_fa_check_circle 
        , Html.p [] [ Html.text t.name ]
        , Html.button [] [ Html.text "unpublish" ]
        ]
    Pending ->   
        [ FA.fas_fa_sync_alt_rolls
        , Html.p [] [ Html.text t.name ] 
        ]
    Errored ->  
        [ FA.fas_fa_exclamation_triangle
        , Html.p [] [ Html.text ("Submission failed for " ++ t.name) ]
        , Html.button [] [ Html.text "dismiss" ]
        ]

type alias TimepublishData = (RequestStatus, (List TimepublishSubmission))

timepublishDataView : TimepublishData -> List (Html msg)
timepublishDataView (status, data) =
    case status of
    Submitted -> 
        [ Html.span [] 
          [ Html.button [] [ FA.fas_fa_sync_alt ]
          , Html.h3 [] [ Html.text "Published times: " ]
          ]
        , List.map (Html.li [] << timePayloadView) data |> Html.ul []  ]
    Pending -> 
        [ Html.span [] 
          [ FA.fas_fa_sync_alt_rolls
          , Html.h3 [] [ Html.text "Published times:" ]
          ]
        , List.map (Html.li [] << timePayloadView) data |> Html.ul []  ]
    Errored -> 
        [ Html.span [ ] 
            [ Html.button [] [ FA.fas_fa_exclamation_triangle ]
            , Html.h3 [] [ Html.text "Error fetching times."]
            ]
        , List.map (Html.li [] << timePayloadView) data |> Html.ul []  ]

conflictingTimesView : List String -> List (Html msg)
conflictingTimesView ps =
    case ps of
    [] -> []
    _ -> 
        [ Html.i [] [ Html.text "Conflicting times:" ]
        ,  Html.b [] [ String.join ", " ps |> Html.text ] 
        ]

view : (Msg -> msg) -> Model -> List (Html msg)
view toAppMsg m = 
    [ Html.h2 [] [ Html.text "Publish a time" ]
    , Html.p [] [ Html.text "Switch to current, previous or next week." ]
    , TSLookup.weekPointerControls (toAppMsg << Move) m.weekpointer
      |> Html.span [ Attr.class "weekpointer" ] 
    , Html.p [] [ Html.text "Point out the weekday." ]
    , TSLookup.selectDayControl (toAppMsg << SelectDay) m.daypointer
      |> Html.span [ Attr.class "daypointer" ]
    , TSLookup.timepointerControl (toAppMsg << SelectHour) (toAppMsg << SelectMinute) (toAppMsg << SelectDuration) (toAppMsg << SelectPause) m.timepointer 
      |> Html.span [ Attr.class "timepointer" ]
    , TSLookup.stagedTimeControl (toAppMsg Skip) (toAppMsg << Submit) m.timepointer
      |> Html.span [ Attr.class "stagedTime" ]
    , conflictingTimesView [ "08:00", "09:00" ]
      |> Html.span [ Attr.class "conflictingTimes" ]
    , timepublishDataView (Submitted, mockTimepublish)
      |> Html.span [ Attr.class "timepublishData" ]
    ]
    