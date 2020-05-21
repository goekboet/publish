module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Http
import Html.Attributes exposing (..)
import Hostname exposing (HostnameForm, Hostname, hostnameForm, initHostnameForm, setHandleValue, setNameValue, setError, addHost, addTimesLink)
import Url exposing (Url)
import Route exposing (Route(..), toRoute)
import SessionState exposing (SessionState(..), sessionstateView, signinLink)
import Weekpointer exposing (..)
import Json.Decode as Decode

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
  , weekpointer: Maybe Weekpointer
  }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , route = toRoute url
    , antiCsrf = flags.antiCsrf
    , sessionState = SessionState.init flags.username
    , hostnameSubmission = initHostnameSubmission flags.hostName flags.hostHandle
    , weekpointer = Just initWeekpointer
    }
  , Cmd.none 
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
      let
          wp old = { old | day = d}
      in
        ( { model | weekpointer = Maybe.map wp model.weekpointer }
        , Cmd.none
        )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

renderHostnameForm : Model -> Html Msg
renderHostnameForm m =
  case (m.sessionState, m.hostnameSubmission) of
    (None, _) -> text ""
    (_, Unsubmitted hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf False
    (_, Submitting hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf True
    (_, FailedSubmit hf) -> hostnameForm NameValueChanged HandleValueChanged SubmitHost hf False
    (_, Submitted h) -> addTimesLink h 

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

        HomeRoute ->
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
        
        BookingsRoute -> 
          [ homelink
            , div 
              [ class "content"
              , class "light" 
              ] 
              [ h2 [] [ text "My bookings"] 
              ]
          ]

        PublishRoute _ _ -> 
          [ homelink
            , div 
              [ class "content"
              , class "light" 
              ] 
              [ h2 [] [ text "Publish"] 
              , Maybe.map (weekpointerView DayfocusChanged) m.weekpointer
                |> Maybe.withDefault (text "")
              ]
          ]

view : Model -> Browser.Document Msg
view model =
  { title = "Publish"
  , body =
      [ div [ class "root-view" ] (routeToView model) ]
  }
