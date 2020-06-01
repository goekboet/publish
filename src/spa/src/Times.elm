module Times exposing 
  ( Time
  , decodeTime
  , decodeTimes
  , publishedTimesView
  , listTimes
  , TimeListing
  , TimesListCall
  , initTimesCall
  , recieveTimesCall
  , faultyTimesCall
  , reloadTimesCall
  , updateTimeSubmission
  , confirmPublish
  , errorPublish
  , pendingPublish)

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

pendingPublish : Int -> String -> String -> String -> Int -> Time
pendingPublish id start day name dur = 
  { id = id
  , start = start
  , day = day
  , end = id + dur
  , name = name
  , status = SubmissionPending
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

appointmentLink : Time -> String
appointmentLink t = absolute [ "appointment", String.fromInt t.id ] []



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
