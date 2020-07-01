module Route exposing 
    ( Route(..)
    , toRoute
    , routeToUrl
    , loginUrl
    , logoutUrl
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing (Parser, (</>))
import Url.Builder as UrlB

type Route
    = NotFound
    | HomeRoute 
    | BookingsRoute 
    | HostRoute 
    | PublishRoute String 
    | Appointment String 


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| UrlP.parse route url


route : Parser (Route -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomeRoute UrlP.top 
        , UrlP.map BookingsRoute (UrlP.s "bookings")
        , UrlP.map HostRoute (UrlP.s "host")
        , UrlP.map PublishRoute (UrlP.s "publish" </> UrlP.string)
        , UrlP.map Appointment (UrlP.s "appointment" </> UrlP.string) ]

routeToUrl : Route -> String -> String
routeToUrl r wptr = 
    case r of
        HomeRoute -> UrlB.absolute [] [ UrlB.string "wptr" wptr ] 
        BookingsRoute -> UrlB.absolute [ "bookings" ] [ UrlB.string "wptr" wptr ]
        HostRoute -> UrlB.absolute [ "host" ] [ UrlB.string "wptr" wptr ]
        PublishRoute h -> UrlB.absolute [ "publish", h ] [ UrlB.string "wptr" wptr ]
        Appointment start -> UrlB.absolute [ "appointment", start ] [ UrlB.string "wptr" wptr ]
        NotFound -> UrlB.absolute [] [ UrlB.string "wptr" wptr ]
            
loginUrl : Route -> String -> String
loginUrl r wptr =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl r wptr) ]

logoutUrl : Route -> String -> String
logoutUrl r wptr =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl r wptr) ]
