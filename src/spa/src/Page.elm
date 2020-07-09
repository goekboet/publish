module Page exposing 
    ( Page(..)
    , fromUrl
    , toUrl
    , loginUrl
    , logoutUrl
    )

import Url exposing (Url)
import Url.Parser as UrlP exposing (Parser)
import Url.Builder as UrlB

type Page
    = HomePage 
    | BookingsPage 
    | HostPage 
    | PublishPage 
    | AppointmentPage 


fromUrl : Url -> Maybe Page
fromUrl url = UrlP.parse route url

route : Parser (Page -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomePage UrlP.top 
        , UrlP.map BookingsPage (UrlP.s "bookings")
        , UrlP.map HostPage (UrlP.s "host")
        , UrlP.map PublishPage (UrlP.s "publish")
        , UrlP.map AppointmentPage (UrlP.s "appointment") ]

toUrl : Page -> String
toUrl r = 
    case r of
        HomePage -> UrlB.absolute [] [] 
        BookingsPage -> UrlB.absolute [ "bookings" ] []
        HostPage -> UrlB.absolute [ "host" ] []
        PublishPage -> UrlB.absolute [ "publish" ] [] 
        AppointmentPage -> UrlB.absolute [ "appointment" ] [] 
            
loginUrl : Page -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (toUrl r) ]

logoutUrl : Page -> String
logoutUrl r = UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (toUrl r) ] 
    
    
