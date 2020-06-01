port module Timesubmission exposing 
    ( Model
    , idTimeSubmission
    , timeSubmissionId
    , init
    , TimePublish
    , Msg(..)
    , update
    , submitTime
    , nextTimeSubmission
    , view
    )

import Html exposing (Html, span, p, input, button, text, li, ul, a, div, h3, i)
import Html.Attributes as Attr exposing (class, type_, min, max, value, step, id )
import Html.Events exposing (onInput, onClick)
import Http exposing (Error)
import Url.Builder exposing (absolute)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)

port idTimeSubmission : Model -> Cmd a
port timeSubmissionId : (Model -> msg) -> Sub msg

type alias Model =
  { start: String
  , day: String
  , dur: Int
  , pause: Int
  , id: Maybe Int
  , name: Maybe String
  }

init : String -> Model
init d = { start = "08:00", day = d, dur = 45, pause = 15, id = Nothing, name = Nothing }

setStart : Model -> String -> Model
setStart ts start = { ts | start = start }

setDur : Model -> String -> Model
setDur ts dur = Maybe.map (\x -> { ts | dur = x }) (String.toInt dur) |> Maybe.withDefault ts

setPause : Model -> String -> Model
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

nextTimeSubmission : Model -> Model
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

submitTime : (Result Error TimePublish -> msg) -> Model -> Cmd msg
submitTime response submission =
  case (submission.id, submission.name) of
      (Just id, Just name) ->
        Http.post
          { url = absolute [ "api", "times"] []
          , body = Http.jsonBody (toTimePayload id name (id + submission.dur))
          , expect = Http.expectJson response fromTimePayload
          }
      _ -> Cmd.none

type Msg
    = StartChange String
    | DurChange String
    | PauseChange String
    | TimeSubmissionIdentified Model
    | SubmitTime

update : Msg -> Model -> (Model, Cmd msg)
update msg model =
    case msg of
        StartChange s ->
            let
                newSubmission = setStart model s
            in
            ( newSubmission 
            , idTimeSubmission newSubmission
            )
      
        DurChange s -> 
            ( setDur model s 
            , Cmd.none
            )
          
        PauseChange s -> 
            ( setPause model s 
            , Cmd.none
            )

        TimeSubmissionIdentified t -> 
            ( t
            , Cmd.none
            )

        SubmitTime -> (model, Cmd.none)

-- submissionIdentified : Sub Msg
-- submissionIdentified = timeSubmissionId TimeSubmissionIdentified

view : (Msg -> msg) -> Model -> Html msg
view wrap t =
    span [ class "addTime"] 
        [ p [] [ text "start"]
        , input [ type_ "time", onInput (wrap << StartChange), value t.start ] []
        , p [] [ text "duration" ]
        , input [ type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt t.dur), onInput (wrap << DurChange), step "5" ] []
        , p [] [ text "pause" ]
        , input [type_ "number", Attr.min "1", Attr.max "999", value (String.fromInt t.pause), onInput (wrap << PauseChange), step "5" ] []
        , button [ onClick (wrap SubmitTime) ] [ text "publish"]
        ]
            
    