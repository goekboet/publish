module Times exposing (addTimeView, Time, publishedTimesView, mockedTimes)

import Html exposing (Html, span, p, input, button, text, li, ul, a)
import Html.Attributes as Attr exposing (class, type_, href, min, max )
import Url.Builder exposing (absolute)


type alias Time =
    { id : String
    , start : String
    , booked: Bool
    , dur: Int
    , name: String
    }

appointmentLink : Time -> String
appointmentLink t = absolute ["appointment", t.id ] []

timeView : Time -> Html msg
timeView t =
    if t.booked
    then li [] 
           [ text t.name
           , a [ href (appointmentLink t)] [ text " Go to appointment."]
           ]
    else li []  
           [ text t.name 
           , button [] [ text "Unpublish" ]
           ]

addTimeView : Html msg
addTimeView =
    span [ class "addTime"] 
        [ p [] [ text "start"]
        , input [ type_ "time" ] []
        , p [] [ text "duration" ]
        , input [ type_ "number", Attr.min "1", Attr.max "999" ] []
        , p [] [ text "pause" ]
        , input [type_ "number", Attr.min "1", Attr.max "999" ] []
        , button [] [ text "publish"]
        ]

publishedTimesView : List Time -> Html msg
publishedTimesView ts =
    ul [ class "publishedTimes" ] (List.map timeView ts)


mockedTimes : List Time
mockedTimes =
    [ { id = "10"
      , start = "08:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 08:00 - 08:45"
      }
    , { id = "20"
      ,start = "09:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 09:00 - 09:45"
      }
    , { id = "30"
      , start = "10:00"
      , booked = True
      , dur = 45
      , name = "mon 18:th may 2020, w21 10:00 - 10:45"
      }
    , { id = "40"
      , start = "11:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 11:00 - 11:45"
      }
    , { id = "50"
      , start = "13:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 13:00 - 13:45"
      }
    , { id = "60"
      , start = "14:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 14:00 - 14:45"
      }
    , { id = "70"
      , start = "15:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 15:00 - 15:45"
      }
    , { id = "80"
      , start = "16:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 16:00 - 16:45"
      }
    , { id = "90"
      , start = "17:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 17:00 - 16:45"
      }
    , { id = "100"
      , start = "18:00"
      , booked = False
      , dur = 45
      , name = "mon 18:th may 2020, w21 18:00 - 16:45"
      }
    ]
