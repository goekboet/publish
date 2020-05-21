module Bookings exposing (bookingsView, mockedbookings)

import Html exposing (Html, h3, ul, li, text, a)
import Html.Attributes exposing (href)
import Url.Builder exposing (absolute)

type alias Booking =
    { link: String
    , name: String 
    }

appointmentlink : Booking -> String
appointmentlink b = absolute [ "appointment", b.link ] []


bookingView : Booking -> Html msg
bookingView b =
    li [] [a [ href (appointmentlink b)] [ text b.name ]]

bookingsView : List Booking -> Html msg
bookingsView bs =
    ul [] (List.map bookingView bs)

mockedbookings : List Booking
mockedbookings = 
    [ { name = "08:00 with Caio Sims", link = "10" }
    , { name = "08:00 with Kashif Lowery", link = "20" }
    , { name = "08:00 with Rebekah Stone", link = "30" }
    ]


