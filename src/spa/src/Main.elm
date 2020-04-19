module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import HomeLink exposing (homelink, homeLinkStyle)
import Model exposing (Msg(..))
import Route exposing (Route(..), toRoute)
import SessionState exposing (SessionState(..), sessionstateView, sessionStateStyle)


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
  }

type alias Model =
  { key : Nav.Key
  , route : Route
  , antiCsrf : AntiCsrfToken
  , sessionState : SessionState
  }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  ( { key = key
    , route = toRoute url
    , antiCsrf = flags.antiCsrf
    , sessionState = SessionState.init flags.username
    }
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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none



-- VIEW

routeToView : Model -> List (Html Msg)
routeToView m =
    case m.route of
        NotFound ->
            [ div [ class "component" ] [ p [] [ text "not found" ] ] 
            ]

        HomeRoute ->
            [ div homeLinkStyle homelink 
            , div sessionStateStyle (sessionstateView m.route m.antiCsrf m.sessionState)
            ]

view : Model -> Browser.Document Msg
view model =
  { title = "Publish"
  , body =
      [ div [ class "root-view" ] (routeToView model) ]
  }
