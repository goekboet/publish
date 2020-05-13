module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Hostname exposing (HostnameForm, hostnameForm, initHostnameForm, setHandleValue, setNameValue, setError, addHost, addTimesLink)
import Url
import HomeLink exposing (homelink, homeLinkStyle)
import Model exposing (Msg(..))
import Route exposing (Route(..), toRoute)
import SessionState exposing (SessionState(..), sessionstateView, sessionStateStyle, signinLink)


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
  | Submitted Model.Hostname

getCurrentPublisher : HostnameSubmission -> Maybe Model.Hostname
getCurrentPublisher s =
  case s of
    Submitted h -> Just h
    _ -> Nothing

initHostnameSubmission : Maybe String -> Maybe String -> HostnameSubmission
initHostnameSubmission name handle =
  Maybe.map2 Model.Hostname name handle
  |> Maybe.map Submitted
  |> Maybe.withDefault (Unsubmitted initHostnameForm)
  

type alias Model =
  { key : Nav.Key
  , route : Route
  , antiCsrf : AntiCsrfToken
  , sessionState : SessionState
  , hostnameSubmission : HostnameSubmission
  }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , route = toRoute url
    , antiCsrf = flags.antiCsrf
    , sessionState = SessionState.init flags.username
    , hostnameSubmission = initHostnameSubmission flags.hostName flags.hostHandle}
  , Cmd.none 
  )



-- UPDATE

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
      ( { model | route = toRoute url }
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
                          , addHost hf)
        _              -> (model, Cmd.none)




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

renderHostnameForm : Model -> Html Msg
renderHostnameForm m =
  case (m.sessionState, m.hostnameSubmission) of
    (None, _) -> text ""
    (_, Unsubmitted hf) -> hostnameForm hf False
    (_, Submitting hf) -> hostnameForm hf True
    (_, FailedSubmit hf) -> hostnameForm hf False
    (_, Submitted h) -> addTimesLink h 

topView : Model -> Html Msg
topView m =
  div [ class "topview" ] 
    [ div [] homelink 
    , div [] (sessionstateView m.route m.antiCsrf m.sessionState)
    ]

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
      [ h3 [] [ text "Broken link" ]
      , notFoundText ]

routeToView : Model -> List (Html Msg)
routeToView m =
    case m.route of
        NotFound ->
            [ topView m
            , notFoundView 
            ]

        HomeRoute ->
            [ topView m
            , signinLink m.route m.antiCsrf m.sessionState 
            , renderHostnameForm m
            ]

view : Model -> Browser.Document Msg
view model =
  { title = "Publish"
  , body =
      [ div [ class "root-view" ] (routeToView model) ]
  }
