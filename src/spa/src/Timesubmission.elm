port module Timesubmission exposing 
    ( Model
    , idTimeSubmission
    , timeSubmissionId
    , timelistingFormatted
    , init
    , Msg(..)
    , update
    , view
    , listTimes
    , decodeTimes
    , publishedTimesView)

import Html exposing (Html, span, p, input, button, text, li, ul, a, div, h3, i)
import Html.Attributes as Attr exposing (class, type_, min, max, value, step, id )
import Html.Events exposing (onInput, onClick)
import Http exposing (Error)
import Url.Builder exposing (absolute)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Route exposing (Route(..))

port idTimeSubmission : Submission -> Cmd a
port timeSubmissionId : (Submission -> msg) -> Sub msg
port formatTimeListing : (List TimeListing) -> Cmd a
port timelistingFormatted : (Value -> msg) -> Sub msg

type alias Model =
    { submission : Submission
    , listing : Listing
    }

type alias Submission =
  { start: String
  , day: String
  , dur: Int
  , pause: Int
  , id: Maybe Int
  , name: Maybe String
  , conflicts : List String
  }

submittable : Submission -> Bool
submittable s =
  case s.id of
    Just _ -> List.length s.conflicts == 0
    _ -> False

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

decodeTime : Decoder Time
decodeTime =
  Decode.map6 Time
    (Decode.field "id" Decode.int)
    (Decode.field "start" Decode.string)
    (Decode.field "day" Decode.string)
    (Decode.field "end" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "status" (Decode.string |> Decode.andThen decodeSubmissionStatus))

decodeTimes : Decoder (List Time)
decodeTimes = Decode.list decodeTime

type Listing 
  = Pending (List Time)
  | Received (List Time)
  | Faulty (List Time)

init : String -> Model
init d = 
    let
        submission = { start = "08:00", day = d, dur = 45, pause = 15, id = Nothing, name = Nothing, conflicts = [] }
    in
        { submission = submission
        , listing = Pending []
        }
    


setStart : Submission -> String -> Submission
setStart ts start = { ts | start = start }

setDur : Submission -> String -> Submission
setDur ts dur = Maybe.map (\x -> { ts | dur = x }) (String.toInt dur) |> Maybe.withDefault ts

setPause : Submission -> String -> Submission
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

nextTimeSubmission : Submission -> Submission
nextTimeSubmission t = 
  let
      next = 
        toMinutes t.start + t.dur + t.pause
        |> fromMinutes
  in
    { t | start = next, id = Nothing, name = Nothing }

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

toTimePayload : Int -> String -> Int -> Value
toTimePayload start name end =
  Encode.object
    [ ("Start", Encode.int start)
    , ("Name", Encode.string name)
    , ("End", Encode.int end)
    ]

submitTime : (Result Error TimePublish -> msg) -> Submission -> Cmd msg
submitTime response submission =
  case (submission.id, submission.name) of
      (Just id, Just name) ->
        Http.post
          { url = absolute [ "api", "times"] []
          , body = Http.jsonBody (toTimePayload id name (id + submission.dur))
          , expect = Http.expectJson response fromTimePayload
          }
      _ -> Cmd.none

updateTimeSubmission : Time -> Listing -> Listing
updateTimeSubmission t c =
  case c of
    Pending ts -> Pending (t :: List.filter (\x -> x.id /= t.id) ts)
    Received ts -> Received (t :: List.filter (\x -> x.id /= t.id) ts)
    Faulty ts -> Faulty (t :: List.filter (\x -> x.id /= t.id) ts)

pendingPublish : Int -> String -> String -> String -> Int -> Time
pendingPublish id start day name dur = 
  { id = id
  , start = start
  , day = day
  , end = id + dur
  , name = name
  , status = SubmissionPending
  }

mapTimesCall : (Time -> Time) -> Listing -> Listing
mapTimesCall fn c =
  case c of
    Pending ts -> List.map fn ts |> Pending
    Received ts -> List.map fn ts |> Received
    Faulty ts -> List.map fn ts |> Faulty

updateTimePublish : Int -> TimeSubmissionStatus -> Time -> Time
updateTimePublish id s t =
  if id == t.id
      then { t | status = s }
      else t 

errorPublish : Int -> Listing -> Listing     
errorPublish t c =
  mapTimesCall (updateTimePublish t SubmissionErrored) c

confirmPublish : Int -> Listing -> Listing
confirmPublish t c =
  mapTimesCall (updateTimePublish t Published) c 

isAuthError : Http.Error -> Bool
isAuthError e =
  case e of
    Http.BadStatus 401 -> True
    _ -> False

faultyTimesCall : Listing -> Listing
faultyTimesCall c =
  case c of 
    Pending ts -> Faulty ts
    Received ts -> Faulty ts
    Faulty ts -> Faulty ts

recieveTimesCall : List Time -> Listing
recieveTimesCall ts = Received ts

reloadTimesCall : Listing -> Listing
reloadTimesCall c =
  case c of
    Pending ts -> Pending ts
    Received ts -> Pending ts
    Faulty ts -> Faulty ts

listTimes : (Msg -> msg) -> (Int, Int) -> Cmd msg
listTimes toAppMsg window =
  let
      windowq (f, t) = 
        [ Url.Builder.int "from" f
        , Url.Builder.int "to" t
        ]
  in
  Http.get
    { url = absolute [ "api", "times"] (windowq window)
    , expect = Http.expectJson (toAppMsg << TimeListingReceived) (Decode.list decodeTimeListing)
    }

type alias TimeListing =
  { start : Int
  , name : String
  , end : Int
  , booked : Bool
  }

recordConflicts : List TimeListing -> Submission -> Submission
recordConflicts ts s =
  let
      conflicts start end = 
        List.filter (\x -> start <= x.end && end >= x.start) ts
        |> List.map .name
  in
    case s.id of
      Just start -> { s | conflicts = conflicts start (start + s.dur) }
      _ -> s 

decodeTimeListing : Decoder TimeListing
decodeTimeListing = 
  Decode.map4 TimeListing
    (Decode.field "start" Decode.int)
    (Decode.field "name" Decode.string)
    (Decode.field "end" Decode.int)
    (Decode.field "booked" Decode.bool)

type Msg
    = StartChange String
    | DurChange String
    | PauseChange String
    | TimeSubmissionIdentified Submission
    | SubmitTime
    | TimePublished Int (Result Http.Error TimePublish)
    | TimeListingReceived (Result Http.Error (List TimeListing))
    | TimeListingFormatted (Result Decode.Error (List Time))
    | ReloadTimelisting

update : (Msg -> msg) -> (Int, Int) -> Msg -> Model -> Maybe (Model, Cmd msg)
update toAppMsg window msg model =
    case msg of
        StartChange s ->
            let
                newSubmission = setStart model.submission s
            in
            Just
            ( { model | submission = newSubmission } 
            , idTimeSubmission newSubmission
            )
      
        DurChange s -> 
            Just
            ( { model 
              | submission = setDur model.submission s } 
            , Cmd.none
            )
          
        PauseChange s -> 
            Just
            ( { model | submission = setPause model.submission s } 
            , Cmd.none
            )

        TimeSubmissionIdentified t ->
            Just 
            ( { model | submission = t }
            , Cmd.none
            )

        SubmitTime -> 
            let
                newSubmission = nextTimeSubmission model.submission
                pending = 
                    case (model.submission.id, model.submission.name) of
                      (Just id, Just name) -> Just
                        { id = id
                        , start = model.submission.start
                        , day = model.submission.day
                        , name = name
                        , dur = model.submission.dur
                        }
                      _ -> Nothing
            in
            Just
            ( { model 
              | submission = newSubmission 
              , listing = Maybe.map
                (\x -> updateTimeSubmission (pendingPublish x.id x.start x.day x.name x.dur) model.listing ) pending
                |> Maybe.withDefault model.listing
              }
            , Cmd.batch
              [ idTimeSubmission newSubmission
              , Maybe.map
                (\x -> submitTime (toAppMsg << TimePublished x.id) model.submission) pending
                |> Maybe.withDefault Cmd.none
              ]
            )

        TimePublished _ (Ok t) ->
            Just
            ( { model
              | listing = confirmPublish t.start model.listing
              }
            , Cmd.none)

        TimePublished id (Err e) ->
          if isAuthError e
          then Nothing 
          else Just
            ( { model 
              |  listing = errorPublish id model.listing 
              }
            , Cmd.none)

        TimeListingReceived (Err e) -> 
          if isAuthError e
          then Nothing
          else Just
            ( { model | listing = faultyTimesCall model.listing
              }
            , Cmd.none 
            )

        TimeListingReceived (Ok ts) -> 
          let
            s = recordConflicts ts model.submission    
          in
          Just
          ( { model | submission = s } 
          , formatTimeListing ts 
          )

        TimeListingFormatted (Ok ts) ->
          Just 
          ( { model | listing = recieveTimesCall ts }
          , Cmd.none
          )

        TimeListingFormatted (Err e) ->
          Just
          ( { model | listing = faultyTimesCall model.listing }
          , Cmd.none
          )

        ReloadTimelisting -> 
          Just
          ( { model | listing = reloadTimesCall model.listing }
          , listTimes toAppMsg window 
          )

view : (Msg -> msg) -> Submission -> Html msg
view wrap t =
    div [] 
      (span [ class "addTime"] 
        [ p [] [ text "start"]
        , input [ type_ "time", onInput (wrap << StartChange), value t.start ] []
        , p [] [ text "duration" ]
        , input [ type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt t.dur), onInput (wrap << DurChange), step "5" ] []
        , p [] [ text "pause" ]
        , input [type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt t.pause), onInput (wrap << PauseChange), step "5" ] []
        , button [ onClick (wrap SubmitTime), Attr.disabled (submittable t |> not) ] [ text "publish"]
        ] :: List.map (\x -> span [] [ text ("conflicts with " ++ x)]) t.conflicts)

timeView : Time -> Html msg
timeView t =
  case t.status of
    SubmissionPending -> li [ class "submissionPending" ] [ text t.name ]
    SubmissionErrored -> li [] 
      [ i [ class "fas", class "fa-exclamation-triangle" ] [] 
      , text (" We failed to publish time: " ++ t.name) ]
    Published -> li [] [ text t.name ]
    Booked -> text ""

publishedTimesView : (Msg -> msg) -> String -> Listing -> Html msg
publishedTimesView toAppMsg day t =
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
            , a [ onClick (toAppMsg ReloadTimelisting) ] [ text "try again" ]
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
            
    