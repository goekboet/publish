module Appointments exposing 
    ( Model
    , Msg
    , init
    , listAppointments
    , update
    , subscribe
    , view
    )

import Html exposing (Html, ul, li, text, a)
import Html.Attributes as Attr exposing (href)
import Html.Events as Event
import Http 
import Json.Decode as Json exposing (Decoder)
import FontAwesome as FA
import Url.Builder as UrlB exposing (absolute)
import Weekpointer as WP exposing (Weekpointer)
import Hostname exposing (init)

type alias Appointment =
    { start : Int
    , host : String
    , name : String
    , end : Int
    , booker : String  
    }

fromJson : Decoder Appointment
fromJson =
  Json.map5 Appointment
    (Json.field "start" Json.int)
    (Json.field "host" Json.string)
    (Json.field "name" Json.string)
    (Json.field "end" Json.int)
    (Json.field "booker" Json.string)

type ListStatus 
    = Pending
    | Received
    | Errored

type alias AppointmentsData = (ListStatus, List Appointment)

setPending : AppointmentsData -> AppointmentsData
setPending (_, appts) = (Pending, appts)

setReceived : List Appointment -> AppointmentsData
setReceived appts = ( Received, appts )

setErrored : AppointmentsData -> AppointmentsData
setErrored (_, appts) = (Errored, appts)

type alias Model =
    { weekpointer : Weekpointer 
    , appointments : AppointmentsData
    }

init : Weekpointer -> Model
init wp = 
    { weekpointer = wp
    , appointments = (Pending, [])
    }

type Msg 
    = Move (Maybe Int)
    | New Weekpointer
    | List
    | AppointmentReceived (Result Http.Error (List Appointment))

listAppointments : (Msg -> msg) -> Weekpointer -> Cmd msg
listAppointments toApp wp =
  let
      toQ (f, t) = 
        [ UrlB.int "from" f
        , UrlB.int "to" t
        ]
      q = 
        WP.getWeekWindow wp
        |> toQ

      url = UrlB.absolute [ "api", "appointments" ] q
  in
  Http.get
  { url = url
  , expect = Http.expectJson (toApp << AppointmentReceived) (Json.list fromJson)}

update : Model -> Msg -> (Msg -> msg) -> (Model, Cmd msg)
update m cmd toApp =
    case cmd of
       Move ts -> (m, WP.moveWeekpointer ts)
       New wp -> 
        ( { m | weekpointer = wp }
        , listAppointments toApp wp )
       AppointmentReceived (Ok appts) -> 
        ( { m | appointments = setReceived appts }
        , Cmd.none)
       AppointmentReceived (Err _) -> 
        ( { m | appointments = setErrored m.appointments }
        , Cmd.none)
       List -> 
        ( { m | appointments = setPending m.appointments}
        , listAppointments toApp m.weekpointer)

subscribe : (Msg -> msg) -> Sub msg 
subscribe toAppmsg =
    WP.newWeekpointer (toAppmsg << New << .week)

weekPointerControls : (Maybe Int -> msg) -> Weekpointer -> List (Html msg) 
weekPointerControls toMsg { current, previous, next } =
    [ Html.h4 [] [ Html.text "Switch to current, previous or next week." ]
    , Html.span []
      [ Html.button 
        [ Event.onClick (Nothing |> toMsg) ] 
        [ FA.fas_fa_chevron_circle_down ]
      , Html.button 
        [ Event.onClick (previous.ts |> Just |> toMsg) ] 
        [ FA.fas_fa_arrow_alt_circle_left ]
      , Html.button 
        [ Event.onClick (next.ts |> Just |> toMsg) ] 
        [ FA.fas_fa_arrow_alt_circle_right ]
      , Html.label [] [ Html.text current.name ]
      ]
    ]

refresh : (Msg -> msg) -> AppointmentsData -> Html msg
refresh toApp (s, _) =
    Html.span [ Attr.class "appointmentsRefresh" ]
    [ Html.label [] [ Html.text "appointments:" ]
    , Html.button 
      [ Event.onClick (toApp List)
      , Attr.disabled (s == Pending)] 
      [ Html.text "refresh" ]]

appointmentsView : AppointmentsData -> Html msg
appointmentsView (_, appts) =
  let
      toListItem a =
        Html.li [] 
        [ FA.far_fa_calendar_check
        , Html.label [] [ Html.text a.name]
        ]
  in
    List.map toListItem appts |> Html.ul [ Attr.class "appointmentsList" ]

view : (Msg -> msg) -> Model -> List (Html msg)
view toApp model =
    [ Html.h2 [] [ Html.text "Appointments"]
    , Html.p [] [ Html.text "Times that folk have booked shows up here."]
    , weekPointerControls (toApp << Move) model.weekpointer
      |> Html.span [ Attr.class "weekpointer" ] 
    , refresh toApp model.appointments
    , Html.hr [ Attr.class "line" ] []
    , appointmentsView model.appointments
    ]


