module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing ( Html, div, h1, h2, text, p, a, button)
import Http exposing (Error)
import Html.Attributes exposing (class, href)
import Hostname as HN
import Url exposing (Url)
import Page exposing (Page(..))
import SessionState as SS
import Appointments  exposing (view)
import Url.Builder exposing (absolute)
import FontAwesome as FA
import Publish
import Weekpointer as WP exposing (Weekpointer)

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
  , tsLookup: WP.TsLookup
  }

type alias Model =
  { key : Nav.Key
  , page : Maybe Page
  , sessionState : SS.Model
  , hostnameSubmission : HN.Model
  , publish : Publish.Model
  , appointments : Appointments.Model
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
      route = Page.fromUrl url

      fetchTimes = 
        case (route, flags.hostHandle) of
        (Just Page.PublishPage, Just _ ) -> True
        _                                -> False
      sessionState = SS.init flags.username (Just flags.antiCsrf)

      publishModel = Publish.init flags.tsLookup fetchTimes
  in
  ( { key = key
    , page = route
    , sessionState = sessionState
    , hostnameSubmission = HN.init flags.hostName flags.hostHandle
    , publish = publishModel
    , appointments = Appointments.init flags.tsLookup.week
    }
  , case route of
    Just HomePage      -> Cmd.none
    Just AppointmentsPage  -> 
      Appointments.listAppointments AppointmentsMessage flags.tsLookup.week
    Just HostPage      -> Cmd.none
    Just PublishPage   -> 
      if fetchTimes
      then Publish.listTimes PublishMessage publishModel.weekpointer
      else Cmd.none 
    Nothing       -> Cmd.none
  )
 


-- UPDATE

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | HostSubmissionUpdate HN.Msg
  | GotNewAnticsrf (Result Error String)
  | PublishMessage Publish.Msg
  | AppointmentsMessage Appointments.Msg
  

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
        nRoute = Page.fromUrl url
      in
        ( { model | page = nRoute }
        , case nRoute of
          Just PublishPage -> Cmd.none
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

    GotNewAnticsrf (Ok t) ->
      ( { model 
        | sessionState = SS.antiCsrfRefreshed model.sessionState t
        }
      , Cmd.none)

    GotNewAnticsrf (Err _) ->
      ( model, Cmd.none )

    PublishMessage message ->
      let
          (pubModel, cmd) = Publish.update model.publish message PublishMessage
      in
        ( { model | publish = pubModel}
        , cmd
        )

    AppointmentsMessage message ->
      let
          (pubModel, cmd) = Appointments.update model.appointments message AppointmentsMessage
      in
        ( { model | appointments = pubModel }
        , cmd
        )

    

    

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions m =
  case m.page of
  Just Page.PublishPage -> Publish.subscribe PublishMessage
  Just Page.AppointmentsPage -> Appointments.subscribe AppointmentsMessage
  _ -> Sub.none




-- VIEW

homelink : SS.Model -> Maybe Page -> Html msg
homelink ss page =
    div [ class "content"
        , class "heavy" 
        , class "home"
        ] 
        [ h1 [] 
          [ a
            [ Page.toUrl HomePage |> href
            ]
            [ text "Publish" ]
          ]
        , if SS.isSignedIn ss
          then SS.formLink ss (Maybe.withDefault HomePage page |> Page.logoutUrl ) FA.fas_fa_sign_out_alt
          else text ""
        ]

welcome : SS.Model -> List (Html msg)
welcome ss = 
 [ h2 [] [ text "Welcome" ]
 , p
   []
   [ text "Publish lets you post times that people can book a videocall with you for. To keep your times apart from everyone elses you need to "
   , SS.formLink ss (Page.loginUrl HomePage) (text "sign in")
   , text " so we know who you are."
   ]
 ]

registerHostname : List (Html msg)
registerHostname =
  [ h2 [] [ text "Register a hostname" ]
  , p [] 
    [ text "Before you can publish times you need a hostname. You can register one "
    , a [ Page.toUrl HostPage |> href ] [ text "here." ]
    ]
  ]

index : List (Html msg)
index =
  [ Html.h2 [] [ text "Welcome" ]
  , Html.span [ class "indexLink" ]
    [ a [ HostPage |> Page.toUrl |> href ]
      [ Html.h3 [] [ text "Host" ]
      , Html.p [] [ text "Review the host you're publishing times as." ]
      ]
    , a [ PublishPage |> Page.toUrl |> href ]
        [ Html.h3 [] [ text "Publish times" ]
        , p [] [ text "Publish times folks can book." ]
        ]
    , a [ AppointmentsPage |> Page.toUrl |> href ]
        [ Html.h3 [] [ text "Appointments" ]
        , p [] [ text "When folk book times you've published they show up here." ]
        ]
    ]
  ]

sessionExpired : SS.Model -> Page -> List (Html msg)
sessionExpired ss page =
  [ h2 [] 
    [ text "Session expired" ] 
  , p [] 
    [ text "You are no longer signed in. To continue your work here you need to "
    , SS.formLink ss (Page.loginUrl page) (text "sign in")
    , text " again."
    ]
  ]

hostnameRequired : List (Html msg)
hostnameRequired =
  [ h2 []
    [ text "Hostname required" ]
  , p []
    [ text "In order to do anything useful on this page you need to "
    , a [ Page.toUrl HostPage |> href ] [ text "register a hostname." ]
    ]
  ]

brokenLinkView : List (Html msg)
brokenLinkView =
  [ h2 []
    [ text "Broken link"]
  , p []
    [ text "The link you clicked is broken. You can always start over from "
    , a [ Page.toUrl HomePage |> href ] [ text "start." ]
    ]
  ]

appointmentsPage : Model -> List (Html Msg)
appointmentsPage m = Appointments.view AppointmentsMessage m.appointments
  

hostPage : Model -> List (Html Msg)
hostPage m = HN.view HostSubmissionUpdate m.hostnameSubmission

publishPage : Model -> List (Html Msg)
publishPage m = Publish.view PublishMessage m.publish

appointmentPage : List (Html Msg)
appointmentPage = 
  [ h2 [] [ text "Appointment" ]
  , p [] [ text "Somename has booked a time at 10:00" ]
  , button [] [ text "Go to meeting" ]
  ]

pageView : Model -> List (Html Msg)
pageView m =
  case ( m.page, SS.isSignedIn m.sessionState, HN.hasHostname m.hostnameSubmission) of
  (Just HomePage,        False, _       ) -> welcome m.sessionState
  (Just HomePage,        True,  Nothing ) -> registerHostname
  (Just HomePage,        True,  Just _  ) -> index
  (Just AppointmentsPage,    False, _       ) -> sessionExpired m.sessionState AppointmentsPage
  (Just AppointmentsPage,    True,  Nothing ) -> hostnameRequired
  (Just AppointmentsPage,    True,  Just _  ) -> appointmentsPage m
  (Just HostPage,        False, _       ) -> sessionExpired m.sessionState HostPage
  (Just HostPage,        True,  _       ) -> hostPage m
  (Just PublishPage,     False, _       ) -> sessionExpired m.sessionState PublishPage
  (Just PublishPage,     True,  Nothing ) -> hostnameRequired
  (Just PublishPage,     True,  Just _  ) -> publishPage m
  (Nothing,              _,     _       ) -> brokenLinkView

view : Model -> Browser.Document Msg
view model =
  { title = "Publish"
  , body =
      [ div 
        [ class "root-view" ] 
        [ homelink model.sessionState model.page
        , div 
          [ class "content"
          , class "light" 
          , class "app-view" 
          ] 
          (pageView model) 
        ] 
      ]
  }
