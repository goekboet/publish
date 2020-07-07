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

routeToUrl : Route -> String
routeToUrl r = 
    case r of
        HomeRoute -> UrlB.absolute [] [] 
        BookingsRoute -> UrlB.absolute [] []
        HostRoute -> UrlB.absolute [] []
        PublishRoute h -> UrlB.absolute [ "publish", h ] []
        Appointment start -> UrlB.absolute [ "appointment", start ] []
        NotFound -> UrlB.absolute [] []
            
loginUrl : Route -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl r) ]

logoutUrl : Route -> String
logoutUrl r =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl r) ]
