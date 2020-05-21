module Route exposing 
    ( Route(..) 
    , toRoute
    , loginUrl
    , logoutUrl
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing (Parser, (</>), (<?>))
import Url.Parser.Query as Query
import Url.Builder as UrlB

type alias WeekParameter = String

type Route
    = NotFound
    | HomeRoute
    | BookingsRoute
    | PublishRoute String (Maybe WeekParameter)
    | Appointment String


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| UrlP.parse route url


route : Parser (Route -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomeRoute UrlP.top 
        , UrlP.map BookingsRoute (UrlP.s "bookings")
        , UrlP.map PublishRoute (UrlP.s "publish" </> UrlP.string <?> Query.string "week")
        , UrlP.map Appointment (UrlP.s "appointment" </> UrlP.string) ]

routeToUrl : Route -> String
routeToUrl _ = "/"

loginUrl : Route -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl r) ]

logoutUrl : Route -> String
logoutUrl r =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl r) ]
