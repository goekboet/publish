port module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import Html.Attributes exposing (..)
import Html.Events
import Hostname exposing (HostnameForm, Hostname, hostnameForm, initHostnameForm, setHandleValue, setNameValue, setError, addHost, addTimesLink)
import Url exposing (Url)
import Route exposing (Route(..), toRoute, getWptr, addWptr, setWptrDay)
import SessionState exposing (SessionState(..), sessionstateView, signinLink)
import Weekpointer exposing (Weekpointer, weekpointerView)
import Times exposing (publishedTimesView, TimeListing, Time, listTimes, TimesListCall, initTimesCall, faultyTimesCall, recieveTimesCall, reloadTimesCall, decodeTime, decodeTimes)
import Timesubmission as TS
import Bookings exposing (bookingsView, mockedbookings)
import Json.Decode as Decode exposing (Decoder)

port nextWeekpointer : (Maybe String) -> Cmd a
port currWeekpointer : (Maybe String) -> Cmd a
port prevWeekpointer : (Maybe String) -> Cmd a
port gotWeekpointer : ((String, Weekpointer) -> msg) -> Sub msg




port formatTimeListing : (List TimeListing) -> Cmd a
port timelistingFormatted : (Value -> msg) -> Sub msg

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
  , antiCsrf : AntiCsrfToken
  , sessionState : SessionState
  , hostnameSubmission : HostnameSubmission
  , weekpointer: Weekpointer
  , timeSubmission : TS.Model
  , timeListing : TimesListCall
  }

loadTimelisting : Route -> (Int, Int) -> (Result Http.Error (List TimeListing) -> msg) -> Cmd msg
loadTimelisting r window response =
  case r of
    PublishRoute _ _ -> listTimes response window
    _ -> Cmd.none

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , route = toRoute url
    , antiCsrf = flags.antiCsrf
    , sessionState = SessionState.init flags.username
    , hostnameSubmission = initHostnameSubmission flags.hostName flags.hostHandle
    , weekpointer = Tuple.second flags.weekpointer
    , timeSubmission = TS.init (Tuple.second flags.weekpointer).day
    , timeListing = initTimesCall
    }
  , Cmd.batch
    [ Nav.pushUrl key (addWptr (toRoute url) (Tuple.first flags.weekpointer))
    , TS.idTimeSubmission (TS.init (Tuple.second flags.weekpointer).day)
    , loadTimelisting (toRoute url) (Tuple.second flags.weekpointer).window TimeListingReceived
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
  | TimePublished Int (Result Http.Error TS.TimePublish)
  | TimeListingReceived (Result Http.Error (List TimeListing))
  | TimeListingFormatted (Result Decode.Error (List Time))
  | ReloadTimelisting

isAuthError : Http.Error -> Bool
isAuthError e =
  case e of
    Http.BadStatus 401 -> True
    _ -> False

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
        , Cmd.none
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
      ( model, currWeekpointer (setWptrDay model.route d))
    
    PrevWeekpointer -> (model, prevWeekpointer (getWptr model.route))

    CurrWeekpointer -> (model, currWeekpointer Nothing)

    NextWeekpointer -> (model, nextWeekpointer (getWptr model.route))

    GotWeekpointer wptr -> 
      ( { model | weekpointer = Tuple.second wptr }
      , Nav.pushUrl model.key (addWptr model.route (Tuple.first wptr)))

    
      

    TimesubmissionUpdate TS.SubmitTime ->
      let
          newSubmission = TS.nextTimeSubmission model.timeSubmission
          pending = 
            case (model.timeSubmission.id, model.timeSubmission.name) of
              (Just id, Just name) -> Just
                { id = id
                , start = model.timeSubmission.start
                , day = model.timeSubmission.day
                , name = name
                , dur = model.timeSubmission.dur
                }
              _ -> Nothing
      in
      ( { model 
        | timeSubmission = newSubmission 
        , timeListing = Maybe.map
          (\x -> Times.updateTimeSubmission (Times.pendingPublish x.id x.start x.day x.name x.dur) model.timeListing ) pending
          |> Maybe.withDefault model.timeListing
        }
      , Cmd.batch
        [ TS.idTimeSubmission newSubmission
        , Maybe.map
          (\x -> TS.submitTime (TimePublished x.id) model.timeSubmission) pending
          |> Maybe.withDefault Cmd.none
        ]
      )

    TimesubmissionUpdate ts -> 
      let
          (m, cmd) = TS.update ts model.timeSubmission
      in
        ( { model | timeSubmission = m}, cmd )

    TimePublished _ (Ok t) ->
      let
          d = Debug.log "publish success: " t
      in
       
      ( { model
        | timeListing = Times.confirmPublish t.start model.timeListing
        }
      , Cmd.none)

    TimePublished id (Err e) -> 
      let
          d = Debug.log "publish error: " e
      in
      
      ( { model 
        | sessionState = if isAuthError e then Stale else model.sessionState
        , timeListing = Times.errorPublish id model.timeListing}
      , Cmd.none)

    TimeListingReceived (Err e) -> 
      ( { model | timeListing = faultyTimesCall model.timeListing
                , sessionState = if isAuthError e then Stale else model.sessionState
        }
      , Cmd.none 
      )

    TimeListingReceived (Ok ts) -> 
      ( model 
      , formatTimeListing ts 
      )

    TimeListingFormatted (Ok ts) -> 
      ( { model | timeListing = recieveTimesCall ts }
      , Cmd.none
      )

    TimeListingFormatted (Err e) ->
      ( { model | timeListing = faultyTimesCall model.timeListing }
      , Cmd.none
      )

    ReloadTimelisting -> 
      ( { model | timeListing = reloadTimesCall model.timeListing }
      , loadTimelisting model.route model.weekpointer.window TimeListingReceived
      )

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.batch
    [ gotWeekpointer GotWeekpointer
    , TS.timeSubmissionId (TimesubmissionUpdate << TS.TimeSubmissionIdentified)
    , timelistingFormatted (TimeListingFormatted << Decode.decodeValue decodeTimes)
    ]



-- VIEW

renderHostnameForm : Model -> Html Msg
renderHostnameForm m =
  case (m.sessionState, m.hostnameSubmission) of
    (None, _) -> text ""
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

homelink : Html msg
homelink =
    div [ class "content"
        , class "heavy" ] 
        [ h1 [] 
          [ a
            [ href "/"
            ]
            [ text "Publish" ]
          ]
        ]

bookingsLink : Model -> Html Msg
bookingsLink m =
    case m.sessionState of
      Fresh _ -> 
        a [ class "navlink" 
          , Html.Attributes.href "/bookings" 
          ] 
          [ h2 [] [ text "My bookings" ]
          , p [] [ text "When you publish times and some user books it, it shows up here." ] 
          ]
      _ -> text ""

routeToView : Model -> List (Html Msg)
routeToView m =
    case m.route of
        NotFound ->
            [ homelink
            , div [ class "content", class "light" ] [notFoundView] 
            ]

        HomeRoute _ ->
            [ homelink
            , div [ class "content", class "light" ] 
            ( List.concat 
                [ signinLink m.route m.antiCsrf m.sessionState
                , [ sessionstateView m.route m.antiCsrf m.sessionState ]
                , [ bookingsLink m ]
                , [ renderHostnameForm m]
                ]
            )
            ]
        
        BookingsRoute _ -> 
          [ homelink
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

        PublishRoute _ _ -> 
          [ homelink
            , div 
              [ class "content"
              , class "light" 
              ] 
              [ h2 [] [ text "Publish a time"]
              , TS.view TimesubmissionUpdate m.timeSubmission
              , h3 [ Html.Events.onClick ReloadTimelisting ] [ text "Published times"]
              , weekpointerView DayfocusChanged PrevWeekpointer CurrWeekpointer NextWeekpointer m.weekpointer
              , case m.sessionState of
                  Fresh _ -> publishedTimesView ReloadTimelisting m.weekpointer.day m.timeListing
                  _ -> SessionState.staleSession m.route m.antiCsrf
              ]
          ]

        Route.Appointment _ _ ->
          [ homelink
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
