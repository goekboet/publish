port module Timesubmission exposing 
    ( Model
    , setDay
    , idTimeSubmission
    , timeSubmissionId
    , timelistingFormatted
    , init
    , Msg(..)
    , update
    , view
    , listTimes
    , decodeTimes
    )

import Html exposing (Html, span, p, input, button, text, li, ul, a, div, h3, h2, i)
import Html.Attributes as Attr exposing (class, type_, min, max, value, step, href )
import Html.Events exposing (onInput, onClick)
import Http exposing (Error)
import Url.Builder exposing (absolute)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import FontAwesome as FA

port idTimeSubmission : (Int, Submission) -> Cmd a
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
  , durMinutes: Int
  , pause: Int
  , id: Maybe Int
  , name: Maybe String
  }

setDay : String -> Model -> Model
setDay day { submission, listing } =
  let
      newSubmission = { submission | day = day }
  in
    { submission = newSubmission
    , listing = listing
    }
    

type TimeSubmissionStatus =
  SubmissionPending
  | SubmissionErrored
  | Published
  | Booked

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
        submission = { start = "08:00", day = d, durMinutes = 45, pause = 15, id = Nothing, name = Nothing }
    in
        { submission = submission
        , listing = Pending []
        }
    


setStart : Submission -> String -> Submission
setStart ts start = { ts | start = start }

setDur : Submission -> String -> Submission
setDur ts dur = Maybe.map (\x -> { ts | durMinutes = x }) (String.toInt dur) |> Maybe.withDefault ts

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
        toMinutes t.start + t.durMinutes + t.pause
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
          , body = Http.jsonBody (toTimePayload id name (id + (submission.durMinutes * 60)))
          , expect = Http.expectJson response fromTimePayload
          }
      _ -> Cmd.none

unpublishTime : Int -> (Result Error () -> msg) -> Cmd msg
unpublishTime id toAppMsg =
  Http.request
    { method = "DELETE"
    , headers = []
    , url = absolute [ "api", "times", String.fromInt id] []
    , body = Http.emptyBody
    , expect = Http.expectWhatever toAppMsg 
    , timeout = Nothing
    , tracker = Nothing
    }

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

deleteTime : Int -> Listing -> Listing
deleteTime id l =
  case l of
    Pending ts -> List.filter (\x -> x.id /= id) ts |> Pending
    Received ts -> List.filter (\x -> x.id /= id) ts |> Received
    Faulty ts -> List.filter (\x -> x.id /= id) ts |> Faulty

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

setPending : Int -> Listing -> Listing
setPending t c =
  mapTimesCall (updateTimePublish t SubmissionPending) c 

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
    | ErrorDismissed Int
    | Unpublish Int
    | Unpublished Int (Result Http.Error ())

update : (Msg -> msg) -> (Int, Int) -> Msg -> Model -> Maybe (Model, Cmd msg)
update toAppMsg window msg model =
    case msg of
        StartChange s ->
            let
                newSubmission = setStart model.submission s
            in
            Just
            ( { model | submission = newSubmission } 
            , idTimeSubmission (Tuple.first window, newSubmission)
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
                        , dur = model.submission.durMinutes
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
              [ idTimeSubmission (Tuple.first window, newSubmission)
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
          Just
          ( model 
          , formatTimeListing ts 
          )

        TimeListingFormatted (Ok ts) ->
          Just 
          ( { model | listing = recieveTimesCall ts }
          , Cmd.none
          )

        TimeListingFormatted (Err _) ->
          Just
          ( { model | listing = faultyTimesCall model.listing }
          , Cmd.none
          )

        ReloadTimelisting -> 
          Just
          ( { model | listing = reloadTimesCall model.listing }
          , listTimes toAppMsg window 
          )

        ErrorDismissed id ->
          Just
          ( { model | listing = deleteTime id model.listing }
          , listTimes toAppMsg window ) 

        Unpublish id ->
          Just 
            ( { model | listing = setPending id model.listing }
            , unpublishTime id (toAppMsg << Unpublished id) )

        Unpublished id (Ok _)->
          Just
          ( { model | listing = deleteTime id model.listing }
          , Cmd.none ) 

        Unpublished id (Err e)->
          if isAuthError e
          then Nothing 
          else Just
            ( { model 
              |  listing = errorPublish id model.listing 
              }
            , Cmd.none)

getTimes : Listing -> List Time 
getTimes l =
  case l of
      Pending ts -> ts
      Received ts -> ts
      Faulty ts -> ts
  

isConflict : Submission -> Time -> Bool
isConflict s t = 
  case s.id of
    Just start -> start <= t.end && (start + s.durMinutes * 60) >= t.id
    _ -> False 

isError : Time -> Bool
isError t =
  case t.status of
    SubmissionErrored -> True
    _ -> False

appointmentLink : Int -> String
appointmentLink id =
  Url.Builder.absolute ["appointment", String.fromInt id ] []

timeView : (Msg -> msg) -> Time -> Html msg
timeView toAppMsg t =
  case t.status of
    SubmissionPending -> 
      li []
          [ i [ class "far", class "fa-check-circle", class "submissionPending" ] []
          , p [] [ text t.name ]
          ]
    SubmissionErrored -> 
      li [] 
          [ FA.fas_fa_exclamation_triangle 
          , p [] [ text t.name ]
          , button 
            [ class "heavy"
            , onClick (ErrorDismissed t.id |> toAppMsg ) 
            ] 
            [ text "dismiss" ]
          ]
    Published -> 
      li [] 
          [ FA.fas_fa_check_circle 
          , p [] [ text t.name ] 
          , button 
            [ class "heavy"
            , onClick (Unpublish t.id |> toAppMsg ) 
            ] 
            [ text "unpublish" ]
          ]
    Booked -> 
      li []
          [ FA.fas_fa_user_circle
          , p [] [ text t.name ]
          , a [ class "heavy", href (appointmentLink t.id) ] [ text "go to booking" ]
          ]

listingErrorMsg : (Msg -> msg) -> Listing -> Html msg
listingErrorMsg toAppmsg l =
  case l of
      Faulty _ -> 
        div [ class "loadfailed" ] 
          [ FA.fas_fa_exclamation_triangle
          , p [ ] 
            [ text "We failed to load your times. You can always " 
            , a [ onClick (toAppmsg ReloadTimelisting) ] [ text "try again" ]
            , text "."
            ] 
          ]
      _ -> text ""

view : (Msg -> msg) -> String -> Model -> Html msg
view wrap day { submission, listing } =
  let
    times = 
      getTimes listing
      |> List.filter (\x -> x.day == day)

    conflicts = 
      times
      |> List.filter (isConflict submission)
      |> List.sortBy .id

    errors =
      times
      |> List.filter isError 
      |> List.sortBy .id 

    published =
      times
      |> List.filter (not << isError) 
      |> List.sortBy .id

    rolls l =
      case l of
        Pending _ -> True
        _ -> False

    submittable = 
      case submission.id of
        Just _ -> List.length conflicts > 0
        _ -> True  
  in
    div [] 
      [ span [ class "addTime"] 
        [ p [] [ text "start"]
        , input [ type_ "time", onInput (wrap << StartChange), value submission.start ] []
        , p [] [ text "duration" ]
        , input [ type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt submission.durMinutes), onInput (wrap << DurChange), step "5" ] []
        , p [] [ text "pause" ]
        , input [type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt submission.pause), onInput (wrap << PauseChange), step "5" ] []
        , button [ onClick (wrap SubmitTime), Attr.disabled submittable ] [ text "publish"]
        ]
      , if List.length conflicts > 0 then h3 [] [ text "Conflicting times." ] else text ""
      , ul [ class "timelisting" ] (List.map (timeView wrap) conflicts)
      , span [ class "timeHeader" ]
        [ FA.fas_fa_sync_alt_control (rolls listing) (wrap ReloadTimelisting)
        , h2 [] [ text "Published times:" ]
        ]
      , listingErrorMsg wrap listing
      , if List.length errors > 0 then h3 [] [ text "Errors" ] else text ""
      , ul [ class "timelisting" ] (List.map (timeView wrap) errors)
      , ul [ class "timelisting" ] (List.map (timeView wrap) published)
      ]