port module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Http exposing (Error)
import Html.Attributes exposing (..)
import Html.Events
import Hostname as HN
import Url exposing (Url)
import Url.Builder as UrlB
import Route exposing (Route(..), toRoute, getWptr, addWptr, setWptrDay)
import SessionState as SS
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

type alias Flags =
  { antiCsrf: SS.AntiCsrfToken
  , username: Maybe SS.Username
  , hostName: Maybe String
  , hostHandle: Maybe String
  , weekpointer: (String, Weekpointer)
  }

type alias Model =
  { key : Nav.Key
  , route : Route
  , sessionState : SS.Model
  , hostnameSubmission : HN.Model
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
    , sessionState = SS.init flags.username (Just flags.antiCsrf)
    , hostnameSubmission = HN.init flags.hostName flags.hostHandle
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
  | HostSubmissionUpdate HN.Msg
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
          PublishRoute _ _ -> Cmd.batch
            [ TS.idTimeSubmission model.times.submission
            , TS.listTimes TimesubmissionUpdate model.weekpointer.window
            ]
          _ -> Cmd.none
        )
    
    HostSubmissionUpdate hn -> 
      let
          r = HN.update HostSubmissionUpdate model.hostnameSubmission hn
      in
        case r of
          Just (m, cmd) -> 
             ( { model 
               | hostnameSubmission = m }
             , cmd
             )
          Nothing       -> 
              ( { model 
                | sessionState = SS.sessionEnded 
                }
              , getNewAntiscrf
              )

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
        , if SS.isSignedIn model.sessionState
          then TS.listTimes TimesubmissionUpdate weekpointer.window
          else Cmd.none
        ]
      )

    TimesubmissionUpdate ts -> 
      let
          r = TS.update TimesubmissionUpdate model.weekpointer.window ts model.times
      in
        case r of
        Just (m, cmd) -> ( { model | times = m }, cmd )
        Nothing -> 
          ( { model 
            | sessionState = SS.sessionEnded 
            }
          , getNewAntiscrf
          )

    GotNewAnticsrf (Ok t) ->
      ( { model 
        | sessionState = SS.antiCsrfRefreshed model.sessionState t
        }
      , Cmd.none)

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

-- addTimesText : Hostname -> Html msg
-- addTimesText h =
--     p [] 
--         [ text "Your publisher name is "
--         , b [] [ text h.name ] 
--         , text "." ]

-- publishUrl : Hostname -> (Maybe String) -> String
-- publishUrl h wptr = 
--     UrlB.relative 
--         [ "publish", h.handle ]
--         (Maybe.map (\x -> [ UrlB.string "wptr" x ]) wptr |> Maybe.withDefault [])

-- addTimesLink : Hostname -> (Maybe String) -> Html msg
-- addTimesLink h wptr =
--     a [ Html.Attributes.href (publishUrl h wptr)] 
--       [ h2 [] [ text "Publish times" ]
--       , addTimesText h ]

-- renderHostnameForm : Model -> Html Msg
-- renderHostnameForm m =
--   case (SS.isSignedIn m.sessionState, m.hostnameSubmission) of
--     (False, _)           -> text ""
--     (_, Unsubmitted hf)  -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf False
--     (_, Submitting hf)   -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf True
--     (_, FailedSubmit hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf False
--     (_, Submitted h)     -> addTimesLink h (getWptr m.route)

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
    case HN.hasHostname m.hostnameSubmission of
      Just _ -> 
        a [ Html.Attributes.href "/bookings" 
          ] 
          [ h2 [] [ text "My bookings" ]
          , p [] [ text "When you publish times and some user books it, it shows up here." ] 
          ]
      _ -> text ""

publishOrAddHostLink : Model -> Html Msg
publishOrAddHostLink m =
  case (SS.isSignedIn m.sessionState, HN.hasHostname m.hostnameSubmission) of
    (False, _)       -> text ""
    (True, Just hn)  ->
      a [ Html.Attributes.href (Route.routeToUrl (PublishRoute hn.handle (Route.getWptr m.route)))
        ]
        [ h2 [] [ text "Publish times" ]
        , p [] [ text ( "You're publishing times as " ++ hn.name ) ]
        ]

    (True, Nothing)  -> 
      a [ Html.Attributes.href (Route.routeToUrl (HostRoute (Route.getWptr m.route)))
        ]
        [ h2 [] [ text "Register a hostname" ]
        , p [] [ text "Before you can publish times you need to register a hostname." ]
        ]

homelink : Model -> Html msg
homelink model =
    div [ class "content"
        , class "heavy" 
        , class "home"
        ] 
        [ h1 [] 
          [ a
            [ href "/"
            ]
            [ text "Publish" ]
          ]
        , if SS.isSignedIn model.sessionState
          then SS.formLink model.sessionState (Route.logoutUrl model.route) (i [class "fas", class "fa-sign-out-alt" ] [])
          else text ""
        ]

signinLink : Model -> List (Html msg)
signinLink m =
    if SS.isSignedIn m.sessionState
    then 
      [ text "" ]
    else 
      [ h2 [] [ text "Login required" ]
      , p
        []
        [ text "Publish lets you post times that people can book a videocall with you for. To keep your times apart from everyone elses you need to "
        , SS.formLink m.sessionState (Route.loginUrl m.route) (text "sign in")
        , text " so we know who you are."
        ]
      ]

routeToView : Model -> List (Html Msg)
routeToView m =
    case m.route of
        NotFound ->
            [ homelink m 
            , div [ class "content", class "light" ] [notFoundView] 
            ]

        HomeRoute _ ->
            [ homelink m
            , div [ class "content", class "light", class "homeLinklist" ] 
            ( List.concat 
                [ signinLink m
                , [ bookingsLink m ]
                , [ publishOrAddHostLink m ]
                ]
            )
            ]
        
        BookingsRoute _ -> 
          [ homelink m
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

        HostRoute _ ->
          [ homelink m
          , div
            [ class "content"
            , class "light"
            ]
            ( if SS.isSignedIn m.sessionState
              then HN.view HostSubmissionUpdate m.hostnameSubmission
              else 
                [ h2 [] [ text "Login required" ]
                , p
                  []
                  [ text "Before you can register a hostname you ned to "
                  , SS.formLink m.sessionState (Route.loginUrl m.route) (text "sign in")
                  , text " so we know who you are."
                  ]
                ]
            )
          ]

        PublishRoute _ wptr -> 
          [ homelink m
            , div 
              [ class "content"
              , class "light" 
              ]
              ( h2 [] [ text "Publish times."] 
              :: p [] [ text "Each time you publish the form will reset to the next time leaving a specified pause. Any published time that has not been booked can be unpublished at any time. Any published time is browseable by the public."]
              :: if SS.isSignedIn m.sessionState 
                 then
                   [ weekpointerView DayfocusChanged PrevWeekpointer CurrWeekpointer NextWeekpointer m.weekpointer
                   , TS.view TimesubmissionUpdate wptr m.weekpointer.day m.times
                   ]
                 else 
                  [ h2 [] [ text "Login required" ]
                  , p
                    []
                    [ text "Your session is expired. You need to "
                    , SS.formLink m.sessionState (Route.loginUrl m.route) (text "sign in")
                    , text " again to continue."
                    ]
                  ]
              )
          ]

        Route.Appointment _ _ ->
          [ homelink m
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
