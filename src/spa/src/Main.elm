port module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Http exposing (Error)
import Html.Attributes exposing (..)
import Html.Events
import Hostname exposing (HostnameForm, Hostname, hostnameForm, initHostnameForm, setHandleValue, setNameValue, setError, addHost)
import Url exposing (Url)
import Url.Builder as UrlB
import Route exposing (Route(..), toRoute, getWptr, addWptr, setWptrDay)
import SessionState exposing (SessionState, sessionstateView, signinLink)
import Weekpointer exposing (Weekpointer, weekpointerView)
-- import Times exposing (publishedTimesView, TimeListing, Time, listTimes, TimesListCall, initTimesCall, faultyTimesCall, recieveTimesCall, reloadTimesCall, decodeTime, decodeTimes)
import Timesubmission as TS
import Bookings exposing (bookingsView, mockedbookings)
import Json.Encode exposing (Value)
import Json.Decode as Decode
import Url.Builder exposing (absolute)


port nextWeekpointer : (Maybe String) -> Cmd a
port currWeekpointer : (Maybe String) -> Cmd a
port prevWeekpointer : (Maybe String) -> Cmd a
port gotWeekpointer : ((String, Weekpointer) -> msg) -> Sub msg


-- MAIN


main : Program Flags Model Msg
main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlChange = UrlChanged
    , onUrlRequest = LinkClicked
    }



-- MODEL

type alias AntiCsrfToken = String
type alias Username = Maybe String

type alias Flags =
  { antiCsrf: AntiCsrfToken
  , username: Username
  , hostName: Maybe String
  , hostHandle: Maybe String
  , weekpointer: (String, Weekpointer)
  }

type HostnameSubmission =
  Unsubmitted HostnameForm
  | Submitting HostnameForm
  | FailedSubmit HostnameForm
  | Submitted Hostname

initHostnameSubmission : Maybe String -> Maybe String -> HostnameSubmission
initHostnameSubmission name handle =
  Maybe.map2 Hostname name handle
  |> Maybe.map Submitted
  |> Maybe.withDefault (Unsubmitted initHostnameForm)
  

type alias Model =
  { key : Nav.Key
  , route : Route
  , antiCsrf : Maybe AntiCsrfToken
  , sessionState : SessionState
  , hostnameSubmission : HostnameSubmission
  , weekpointer: Weekpointer
  , times : TS.Model
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
      times = TS.init (Tuple.second flags.weekpointer).day 
      weekpointer = Tuple.second flags.weekpointer
  in
  ( { key = key
    , route = toRoute url
    , antiCsrf = Just flags.antiCsrf
    , sessionState = flags.username
    , hostnameSubmission = initHostnameSubmission flags.hostName flags.hostHandle
    , weekpointer = weekpointer
    , times = times
    }
  , Cmd.batch
    [ Nav.pushUrl key (addWptr (toRoute url) (Tuple.first flags.weekpointer))
    , case flags.hostHandle of
      Just _ ->  TS.listTimes TimesubmissionUpdate weekpointer.window
      _ -> Cmd.none 
    ] 
  )



-- UPDATE

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | SubmitHost
  | HostAdded (Result Http.Error Hostname)
  | HandleValueChanged String
  | NameValueChanged String
  | DayfocusChanged String
  | PrevWeekpointer
  | CurrWeekpointer
  | NextWeekpointer
  | GotWeekpointer (String, Weekpointer)
  | TimesubmissionUpdate TS.Msg
  | GotNewAnticsrf (Result Error String)
  

getNewAntiscrf : Cmd Msg
getNewAntiscrf = 
  Http.get
    { url = absolute [ "anticsrf" ] []
    , expect = Http.expectString GotNewAnticsrf
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Nav.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Nav.load href )

    UrlChanged url ->
      let
        nRoute = toRoute url
      in
        ( { model | route = nRoute }
        , case nRoute of
          PublishRoute _ _ -> TS.idTimeSubmission model.times.submission
          _ -> Cmd.none
        )
    
    HostAdded (Ok h) -> ({ model | hostnameSubmission = Submitted h }, Cmd.none) 
    HostAdded (Err e) -> 
      case model.hostnameSubmission of
        Submitting hf -> ({ model | hostnameSubmission = FailedSubmit (hf |> setError e) }, Cmd.none)
        _             -> ({ model | hostnameSubmission = FailedSubmit (initHostnameForm |> setError e) }, Cmd.none)
    HandleValueChanged s -> 
      case model.hostnameSubmission of
        Unsubmitted hf -> ( { model 
                            | hostnameSubmission = Unsubmitted (setHandleValue s hf)
                            }
                          , Cmd.none)
        _              -> (model, Cmd.none)

    NameValueChanged s -> 
      case model.hostnameSubmission of
        Unsubmitted hf -> ( { model 
                            | hostnameSubmission = Unsubmitted (setNameValue s hf)
                            }
                          , Cmd.none)
        _              -> (model, Cmd.none)

    SubmitHost -> 
      case model.hostnameSubmission of
        Unsubmitted hf -> ( { model
                            | hostnameSubmission = Submitting hf
                            } 
                          , addHost HostAdded hf)
        _              -> (model, Cmd.none)

    DayfocusChanged d ->
      let
          newTimes = TS.setDay d model.times
          newWeekPointer = Weekpointer.setDay model.weekpointer d
      in
      
      ( { model 
        | times = newTimes
        , weekpointer = newWeekPointer 
        }
      , Cmd.batch 
          [ Maybe.map 
            (\x -> Nav.pushUrl model.key (addWptr model.route x)) (setWptrDay model.route d) 
            |> Maybe.withDefault Cmd.none
          , TS.idTimeSubmission newTimes.submission
          ]
      
      ) 
    
    PrevWeekpointer -> (model, prevWeekpointer (getWptr model.route))

    CurrWeekpointer -> (model, currWeekpointer Nothing)

    NextWeekpointer -> (model, nextWeekpointer (getWptr model.route))

    GotWeekpointer wptr -> 
      let
          weekpointer = Tuple.second wptr
          query = Tuple.first wptr
      in
      
      ( { model | weekpointer = weekpointer }
      , Cmd.batch 
        [ Nav.pushUrl model.key (addWptr model.route query)
        , case model.sessionState of
          Just _ -> TS.listTimes TimesubmissionUpdate weekpointer.window
          _ -> Cmd.none
        ]
      )

    TimesubmissionUpdate ts -> 
      let
          r = TS.update TimesubmissionUpdate model.weekpointer.window ts model.times
      in
        case r of
        Just (m, cmd) -> ( { model | times = m }, cmd )
        Nothing -> 
          ( { model | sessionState = Nothing }
          , getNewAntiscrf
          )

    GotNewAnticsrf (Ok t) ->
      ( { model | antiCsrf = Just t}, Cmd.none)

    GotNewAnticsrf (Err e) ->
      ( model, Cmd.none )

    

    

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ gotWeekpointer GotWeekpointer
    , TS.timeSubmissionId (TimesubmissionUpdate << TS.TimeSubmissionIdentified)
    , TS.timelistingFormatted (TimesubmissionUpdate << TS.TimeListingFormatted << Decode.decodeValue TS.decodeTimes)
    ]



-- VIEW

addTimesText : Hostname -> Html msg
addTimesText h =
    p [] 
        [ text "Your publisher name is "
        , b [] [ text h.name ] 
        , text "." ]

publishUrl : Hostname -> (Maybe String) -> String
publishUrl h wptr = 
    UrlB.relative 
        [ "publish", h.handle ]
        (Maybe.map (\x -> [ UrlB.string "wptr" x ]) wptr |> Maybe.withDefault [])

addTimesLink : Hostname -> (Maybe String) -> Html msg
addTimesLink h wptr =
    a [ Html.Attributes.href (publishUrl h wptr)] 
      [ h2 [] [ text "Publish times" ]
      , addTimesText h ]

renderHostnameForm : Model -> Html Msg
renderHostnameForm m =
  case (m.sessionState, m.hostnameSubmission) of
    (Nothing, _) -> text ""
    (_, Unsubmitted hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf False
    (_, Submitting hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf True
    (_, FailedSubmit hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf False
    (_, Submitted h) -> addTimesLink h (getWptr m.route)

notFoundText : Html Msg
notFoundText = 
  p [] 
    [ text "This link is broken. Please report to the webmaster. There is not really much else you can do but "
    , a [ Html.Attributes.href "/" ] [ text "go back to the beginning."
    ]]

notFoundView : Html Msg
notFoundView =
    div [ class "notFoundView" 
      ] 
      [ h2 [] [ text "Broken link" ]
      , notFoundText ]



bookingsLink : Model -> Html Msg
bookingsLink m =
    case m.hostnameSubmission of
      Submitted _ -> 
        a [ Html.Attributes.href "/bookings" 
          ] 
          [ h2 [] [ text "My bookings" ]
          , p [] [ text "When you publish times and some user books it, it shows up here." ] 
          ]
      _ -> text ""

routeToView : Model -> List (Html Msg)
routeToView m =
    case m.route of
        NotFound ->
            [ SessionState.homelink m.sessionState m.route m.antiCsrf 
            , div [ class "content", class "light" ] [notFoundView] 
            ]

        HomeRoute _ ->
            [ SessionState.homelink m.sessionState m.route m.antiCsrf
            , div [ class "content", class "light", class "homeLinklist" ] 
            ( List.concat 
                [ signinLink m.route m.antiCsrf m.sessionState
                , [ bookingsLink m ]
                , [ renderHostnameForm m]
                ]
            )
            ]
        
        BookingsRoute _ -> 
          [ SessionState.homelink m.sessionState m.route m.antiCsrf
            , div 
              [ class "content"
              , class "light" 
              ] 
              [ h2 [] [ text "My bookings"] 
              , p [] [ text "Any times booked by someone will show up here."]
              , weekpointerView DayfocusChanged PrevWeekpointer CurrWeekpointer NextWeekpointer m.weekpointer
              , bookingsView mockedbookings
              ]
          ]

        PublishRoute _ wptr -> 
          [ SessionState.homelink m.sessionState m.route m.antiCsrf
            , div 
              [ class "content"
              , class "light" 
              ]
              ( h2 [] [ text "Publish times."] 
              :: p [] [ text "Each time you publish the form will reset to the next time leaving a specified pause. Any published time that has not been booked can be unpublished at any time. Any published time is browseable by the public."]
              ::  case m.sessionState of
                Just _ ->
                  [ weekpointerView DayfocusChanged PrevWeekpointer CurrWeekpointer NextWeekpointer m.weekpointer
                  , TS.view TimesubmissionUpdate wptr m.weekpointer.day m.times
                  ]
                _ -> 
                  [ SessionState.staleSession m.route m.antiCsrf ])
          ]

        Route.Appointment _ _ ->
          [ SessionState.homelink m.sessionState m.route m.antiCsrf
            , div 
              [ class "content"
              , class "light" 
              ] 
              [ h2 [] [ text "Appointment" ]
              , p [] [ text "Somename has booked a time at 10:00" ]
              , button [] [ text "Go to meeting" ]
              ]
          ]

view : Model -> Browser.Document Msg
view model =
  { title = "Publish"
  , body =
      [ div [ class "root-view" ] (routeToView model) ]
  }
