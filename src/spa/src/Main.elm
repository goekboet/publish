port module Main exposing (main)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav
import Html exposing ( Html, div, h1, h2, text, p, a, button)
import Http exposing (Error)
import Html.Attributes exposing (class, href)
import Hostname as HN
import Url exposing (Url)
import Page exposing (Page(..))
import SessionState as SS
import Weekpointer as WP
import Timesubmission as TS
import Bookings exposing (bookingsView, mockedbookings)
import Json.Decode as Decode
import Url.Builder exposing (absolute)
import FontAwesome as FA

port moveWeekpointer : (Int, WP.Model) -> Cmd a
port gotWeekpointer : (WP.Model -> msg) -> Sub msg


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
  , weekpointer: WP.Model
  }

type alias Model =
  { key : Nav.Key
  , page : Maybe Page
  , sessionState : SS.Model
  , hostnameSubmission : HN.Model
  , weekpointer: WP.Model
  , times : TS.Model
  }

init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
  let
      times = TS.init flags.weekpointer.day
      route = Page.fromUrl url
      sessionState = SS.init flags.username (Just flags.antiCsrf)

  in
  ( { key = key
    , page = route
    , sessionState = sessionState
    , hostnameSubmission = HN.init flags.hostName flags.hostHandle
    , weekpointer = flags.weekpointer
    , times = times
    }
  , case route of
    Just HomePage      -> Cmd.none
    Just BookingsPage  -> Cmd.none
    Just HostPage      -> Cmd.none
    Just PublishPage   -> 
      case flags.hostHandle of
      Just _ ->  TS.listTimes TimesubmissionUpdate flags.weekpointer.window
      _      -> Cmd.none 
    Just AppointmentPage -> Cmd.none
    Nothing       -> Cmd.none
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
  | GotWeekpointer WP.Model
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
        nRoute = Page.fromUrl url
      in
        ( { model | page = nRoute }
        , case nRoute of
          Just PublishPage -> Cmd.batch
            [ TS.idTimeSubmission (Tuple.first model.weekpointer.window, model.times.submission)
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
      in
      
      ( { model 
        | times = newTimes
        , weekpointer = WP.setDay model.weekpointer d
        }
      , TS.idTimeSubmission (Tuple.first model.weekpointer.window, newTimes.submission)
      ) 
    
    PrevWeekpointer -> (model, moveWeekpointer (-1, model.weekpointer) )

    CurrWeekpointer -> (model, moveWeekpointer ( 0, model.weekpointer) )

    NextWeekpointer -> (model, moveWeekpointer ( 1, model.weekpointer) )

    GotWeekpointer wptr -> 
      ( { model | weekpointer = wptr }
      , case (model.page, model.hostnameSubmission) of
        (Just PublishPage, HN.Submitted _) -> TS.listTimes TimesubmissionUpdate wptr.window
        _ -> Cmd.none
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

    GotNewAnticsrf (Err _) ->
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
  [ a [ HostPage |> Page.toUrl |> href
      , class "indexLink"
      ]
      [ h2 [] [ text "Host" ]
      , p [] [ text "Review the host you're publishing times as." ]
      ]
  , a [ PublishPage |> Page.toUrl |> href
      , class "indexLink"
      ]
      [ h2 [] [ text "Publish times" ]
      , p [] [ text "Publish times folks can book." ]
      ]
  , a [ BookingsPage |> Page.toUrl |> href
      , class "indexLink"
      ]
      [ h2 [] [ text "Bookings" ]
      , p [] [ text "When folk book times you've published they show up here." ]
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

bookingsPage : Model -> List (Html Msg)
bookingsPage m = 
  [ h2 [] [ text "My bookings"] 
  , p [] [ text "Any times booked by someone will show up here."]
  , WP.view DayfocusChanged PrevWeekpointer CurrWeekpointer NextWeekpointer m.weekpointer
  , bookingsView mockedbookings
  ]

hostPage : Model -> List (Html Msg)
hostPage m = HN.view HostSubmissionUpdate m.hostnameSubmission

publishPage : Model -> List (Html Msg)
publishPage m = 
  [ h2 [] [ text "Publish times."] 
  , p [] [ text "Each time you publish the form will reset to the next time leaving a specified pause. Any published time that has not been booked can be unpublished at any time. Any published time is browseable by the public."]
  , WP.view DayfocusChanged PrevWeekpointer CurrWeekpointer NextWeekpointer m.weekpointer
  , TS.view TimesubmissionUpdate m.weekpointer.day m.times
  ]

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
  (Just BookingsPage,    False, _       ) -> sessionExpired m.sessionState BookingsPage
  (Just BookingsPage,    True,  Nothing ) -> hostnameRequired
  (Just BookingsPage,    True,  Just _  ) -> bookingsPage m
  (Just HostPage,        False, _       ) -> sessionExpired m.sessionState HostPage
  (Just HostPage,        True,  _       ) -> hostPage m
  (Just PublishPage,     False, _       ) -> sessionExpired m.sessionState PublishPage
  (Just PublishPage,     True,  Nothing ) -> hostnameRequired
  (Just PublishPage,     True,  Just _  ) -> publishPage m
  (Just AppointmentPage, False, _       ) -> sessionExpired m.sessionState AppointmentPage
  (Just AppointmentPage, True,  Nothing ) -> hostnameRequired
  (Just AppointmentPage, True,  Just _  ) -> appointmentPage
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
          ] 
          (pageView model) 
        ] 
      ]
  }
