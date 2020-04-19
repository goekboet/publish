module Route exposing 
    ( Route(..) 
    , toRoute
    , loginUrl
    , logoutUrl
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing (Parser)
import Url.Builder as UrlB

type Route
    = NotFound
    | HomeRoute 


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| UrlP.parse route url


route : Parser (Route -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomeRoute UrlP.top ]

routeToUrl : Route -> String
routeToUrl _ = "/"

loginUrl : Route -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl r) ]

logoutUrl : Route -> String
logoutUrl r =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl r) ]
