module Times exposing 
  ( addTimeView
  , TimeSubmission
  , initTimeSubmission
  , setStart
  , setDur
  , setPause
  , nextTimeSubmission
  , Time
  , decodeTime
  , decodeTimes
  , publishedTimesView
  , submitTime
  , TimePublish
  , listTimes
  , TimeListing
  , TimesListCall
  , initTimesCall
  , recieveTimesCall
  , faultyTimesCall
  , reloadTimesCall
  , toTime
  , updateTimeSubmission
  , confirmPublish
  , errorPublish)

import Html exposing (Html, span, p, input, button, text, li, ul, a, div, h3, i)
import Html.Attributes as Attr exposing (class, type_, min, max, value, step, id )
import Html.Events exposing (onInput, onClick)
import Url.Builder exposing (absolute)
import Http exposing (Error)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
-- import Debug exposing (Debug)

type alias Time =
    { id : Int
    , start : String
    , day : String
    , end: Int
    , name: String
    , status: TimeSubmissionStatus
    }

encodeTime : Time -> Value
encodeTime t =
  Encode.object
    [ ("id", Encode.int t.id) 
    , ("start", Encode.string t.start)
    , ("end", Encode.int t.end)
    , ("name", Encode.string t.name)
    , ("status", encodeTimeSubmissionStatus t.status)
    ]

decodeTimes : Decoder (List Time)
decodeTimes = Decode.list decodeTime

decodeTime : Decoder Time
decodeTime =
  Decode.map6 Time
    (Decode.field "id" Decode.int)
    (Decode.field "start" Decode.string)
    (Decode.field "day" Decode.string)
    (Decode.field "end" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "status" (Decode.string |> Decode.andThen decodeSubmissionStatus))

type TimeSubmissionStatus =
  SubmissionPending
  | SubmissionErrored
  | Published
  | Booked

encodeTimeSubmissionStatus : TimeSubmissionStatus -> Value
encodeTimeSubmissionStatus ts =
  case ts of
    SubmissionPending -> Encode.string "SubmissionPending"
    SubmissionErrored -> Encode.string "SubmissionErrored"
    Published -> Encode.string "Published"
    Booked -> Encode.string "Booked"


decodeSubmissionStatus : String -> Decoder TimeSubmissionStatus
decodeSubmissionStatus s =
  case s of
    "SubmissionPending" -> Decode.succeed SubmissionPending
    "SubmissionErrored" -> Decode.succeed SubmissionErrored
    "Published" -> Decode.succeed Published
    "Booked" -> Decode.succeed Booked
    _ -> Decode.fail "Unrecognized TimeSubmissionStatus"

timeView : Time -> Html msg
timeView t =
  case t.status of
    SubmissionPending -> li [ class "submissionPending" ] [ text t.name ]
    SubmissionErrored -> li [] 
      [ i [ class "fas", class "fa-exclamation-triangle" ] [] 
      , text (" We failed to publish time: " ++ t.name) ]
    Published -> li [] [ text t.name ]
    Booked -> text ""

type alias TimeSubmission =
  { start: String
  , day: String
  , dur: Int
  , pause: Int
  , id: Maybe Int
  , name: Maybe String
  }

toTime : TimeSubmission -> Maybe Time
toTime ts = 
  case (ts.id, ts.name) of
    (Just identified, Just name) ->
      Just
      { id = identified
      , start = ts.start
      , day = ts.day
      , end = identified + ts.dur
      , name = name
      , status = SubmissionPending
      }
    _ -> Nothing

initTimeSubmission : String -> TimeSubmission
initTimeSubmission d = { start = "08:00", day = d, dur = 45, pause = 15, id = Nothing, name = Nothing }

setStart : TimeSubmission -> String -> TimeSubmission
setStart ts start = { ts | start = start }

setDur : TimeSubmission -> String -> TimeSubmission
setDur ts dur = Maybe.map (\x -> { ts | dur = x }) (String.toInt dur) |> Maybe.withDefault ts

setPause : TimeSubmission -> String -> TimeSubmission
setPause ts pause = Maybe.map (\x -> { ts | pause = x }) (String.toInt pause) |> Maybe.withDefault ts

toMinutes : String -> Int
toMinutes s =
  let
      h str =
        String.split ":" str 
        |> List.head 
        |> Maybe.andThen String.toInt

      m str =
        String.split ":" str
        |> List.drop 1
        |> List.head
        |> Maybe.andThen String.toInt
  in
    case (h s, m s) of
        (Just hour, Just min) -> (hour * 60) + min
        _                     -> 0
            
fromMinutes : Int -> String      
fromMinutes mins =
  let
      h = mins // 60 |> remainderBy 23
      m = remainderBy 60 mins
  in
    String.concat 
      [ String.fromInt h |> String.padLeft 2 '0'
      , ":"
      , String.fromInt m |> String.padLeft 2 '0'
      ]


nextTimeSubmission : TimeSubmission -> TimeSubmission
nextTimeSubmission t = 
  let
      next = 
        toMinutes t.start + t.dur + t.pause
        |> fromMinutes
  in
    { t | start = next, id = Nothing, name = Nothing }
  

appointmentLink : Time -> String
appointmentLink t = absolute [ "appointment", String.fromInt t.id ] []

addTimeView : (String -> msg) -> (String -> msg) -> (String -> msg) -> msg -> TimeSubmission -> Html msg
addTimeView startChange durChange pauseChange submit t =
    span [ class "addTime"] 
        [ p [] [ text "start"]
        , input [ type_ "time", onInput startChange, value t.start ] []
        , p [] [ text "duration" ]
        , input [ type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt t.dur), onInput durChange, step "5" ] []
        , p [] [ text "pause" ]
        , input [type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt t.pause), onInput pauseChange, step "5" ] []
        , button [ onClick submit ] [ text "publish"]
        ]

publishedTimesView : msg -> String -> TimesListCall -> Html msg
publishedTimesView refetch day t =
  let
      pending = li [] [ div [ id "loading" ] [] ]

      todaysTimes d fn ts = 
        List.sortBy .id ts 
        |> List.filterMap
        (\x -> if x.day == d then Just (fn x) else Nothing) 
        

      error = 
        li [ id "timefetcherror" ] 
          [ h3 [] 
            [ i [ class "fas", class "fa-exclamation-triangle" ] []
            , text " We failed to fetch your times"
            ]
          , p [ ] 
            [ text "You can always " 
            , a [ onClick refetch ] [ text "try again" ]
            , text "."
            ] 
          ]
  in
  case t of
    Pending ts -> 
      ul [ class "publishedTimes" ] (pending :: todaysTimes day timeView ts)
    Received ts -> 
      ul [ class "publishedTimes" ] (todaysTimes day timeView ts)
    Faulty ts -> 
      ul [ class "publishedTimes" ] (error :: todaysTimes day timeView ts)
          
type alias TimePublish =
  { start : Int
  , name : String
  , end : Int
  }

updateTimePublish : Int -> TimeSubmissionStatus -> Time -> Time
updateTimePublish id s t =
  if id == t.id
      then { t | status = s }
      else t 

confirmPublish : Int -> TimesListCall -> TimesListCall
confirmPublish t c =
  mapTimesCall (updateTimePublish t Published) c 

errorPublish : Int -> TimesListCall -> TimesListCall     
errorPublish t c =
  mapTimesCall (updateTimePublish t SubmissionErrored) c
    
  

fromTimePayload : Decoder TimePublish
fromTimePayload =
  Decode.map3 TimePublish
    (Decode.field "start" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "end" Decode.int)

toTimePayload : Int -> String -> Int -> Value
toTimePayload start name end =
  Encode.object
    [ ("Start", Encode.int start)
    , ("Name", Encode.string name)
    , ("End", Encode.int end)
    ]

submitTime : (Result Error TimePublish -> msg) -> TimeSubmission -> Cmd msg
submitTime response submission =
  case (submission.id, submission.name) of
      (Just id, Just name) ->
        Http.post
          { url = absolute [ "api", "times"] []
          , body = Http.jsonBody (toTimePayload id name (id + submission.dur))
          , expect = Http.expectJson response fromTimePayload
          }
      _ -> Cmd.none

type TimesListCall 
  = Pending (List Time)
  | Received (List Time)
  | Faulty (List Time)

mapTimesCall : (Time -> Time) -> TimesListCall -> TimesListCall
mapTimesCall fn c =
  case c of
    Pending ts -> List.map fn ts |> Pending
    Received ts -> List.map fn ts |> Received
    Faulty ts -> List.map fn ts |> Faulty
    
initTimesCall : TimesListCall
initTimesCall = Pending []

recieveTimesCall : List Time -> TimesListCall
recieveTimesCall ts = Received ts

faultyTimesCall : TimesListCall -> TimesListCall
faultyTimesCall c =
  case c of 
    Pending ts -> Faulty ts
    Received ts -> Faulty ts
    Faulty ts -> Faulty ts

reloadTimesCall : TimesListCall -> TimesListCall
reloadTimesCall c =
  case c of
    Pending ts -> Pending ts
    Received ts -> Pending ts
    Faulty ts -> Faulty ts

updateTimeSubmission : Time -> TimesListCall -> TimesListCall
updateTimeSubmission t c =
  case c of
    Pending ts -> Pending (t :: List.filter (\x -> x.id /= t.id) ts)
    Received ts -> Received (t :: List.filter (\x -> x.id /= t.id) ts)
    Faulty ts -> Faulty (t :: List.filter (\x -> x.id /= t.id) ts)


type alias TimeListing =
  { start : Int
  , name : String
  , end : Int
  , booked : Bool
  }

decodeTimeListing : Decoder TimeListing
decodeTimeListing = 
  Decode.map4 TimeListing
    (Decode.field "start" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "end" Decode.int)
    (Decode.field "booked" Decode.bool)

listTimes : (Result Error (List TimeListing) -> msg) -> (Int, Int) -> Cmd msg
listTimes response window =
  let
      windowq (f, t) = 
        [ Url.Builder.int "from" f
        , Url.Builder.int "to" t
        ]
  in
  Http.get
    { url = absolute [ "api", "times"] (windowq window)
    , expect = Http.expectJson response (Decode.list decodeTimeListing)
    }
