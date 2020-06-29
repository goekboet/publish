module Route exposing 
    ( Route(..)
    , Wptr
    , getWptr
    , addWptr
    , setWptrDay 
    , toRoute
    , routeToUrl
    , loginUrl
    , logoutUrl
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing (Parser, (</>), (<?>))
import Url.Parser.Query as Query
import Url.Builder as UrlB

type alias Wptr = String

type Route
    = NotFound
    | HomeRoute (Maybe Wptr)
    | BookingsRoute (Maybe Wptr)
    | HostRoute (Maybe Wptr)
    | PublishRoute String (Maybe Wptr)
    | Appointment String (Maybe Wptr)


toRoute : Url -> Route
toRoute url =
    Maybe.withDefault NotFound <| UrlP.parse route url


route : Parser (Route -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomeRoute (UrlP.top <?> Query.string "wptr")
        , UrlP.map BookingsRoute (UrlP.s "bookings" <?> Query.string "wptr")
        , UrlP.map HostRoute (UrlP.s "host" <?> Query.string "wptr")
        , UrlP.map PublishRoute (UrlP.s "publish" </> UrlP.string <?> Query.string "wptr")
        , UrlP.map Appointment (UrlP.s "appointment" </> UrlP.string <?> Query.string "wptr") ]

routeToUrl : Route -> String
routeToUrl r = 
    case r of
        HomeRoute wp -> UrlB.absolute [] (Maybe.map (List.singleton << toQueryParameter) wp |> Maybe.withDefault [] ) 
        BookingsRoute wp -> UrlB.absolute [ "bookings" ] (Maybe.map (List.singleton << toQueryParameter) wp |> Maybe.withDefault [] )
        HostRoute wp -> UrlB.absolute [ "host" ] (Maybe.map (List.singleton << toQueryParameter) wp |> Maybe.withDefault [] )
        PublishRoute h wp -> UrlB.absolute [ "publish", h ] (Maybe.map (List.singleton << toQueryParameter) wp |> Maybe.withDefault [] )
        Appointment start wp -> UrlB.absolute [ "appointment", start ] (Maybe.map (List.singleton << toQueryParameter) wp |> Maybe.withDefault [] )
        NotFound -> UrlB.absolute [] []

getWptr : Route -> Maybe Wptr
getWptr r =
    case r of
        HomeRoute wp -> wp
        BookingsRoute wp -> wp
        PublishRoute _ wp -> wp
        Appointment _ wp -> wp
        _ -> Nothing

toQueryParameter : Wptr -> UrlB.QueryParameter
toQueryParameter wp = UrlB.string "wptr" wp 

addWptr : Route -> Wptr -> String
addWptr r wp =
    case r of
        HomeRoute _ -> routeToUrl (HomeRoute (Just wp))
        BookingsRoute _ -> routeToUrl (BookingsRoute (Just wp))
        HostRoute _ -> routeToUrl (HostRoute (Just wp))
        PublishRoute h _ -> routeToUrl (PublishRoute h (Just wp))
        Appointment s _ -> routeToUrl (Appointment s (Just wp))
        _ -> UrlB.absolute [] []

setWptrDay : Route -> String -> (Maybe String)
setWptrDay r day =
    let
       setday p = String.dropRight 3 p ++ day   
    in
        Maybe.map setday (getWptr r)
            
loginUrl : Route -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (routeToUrl r) ]

logoutUrl : Route -> String
logoutUrl r =
    UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (routeToUrl r) ]
