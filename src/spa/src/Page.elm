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
    | AppointmentsPage 
    | HostPage 
    | PublishPage 


fromUrl : Url -> Maybe Page
fromUrl url = UrlP.parse route url

route : Parser (Page -> a) a
route =
    UrlP.oneOf
        [ UrlP.map HomePage UrlP.top 
        , UrlP.map AppointmentsPage (UrlP.s "appointments")
        , UrlP.map HostPage (UrlP.s "host")
        , UrlP.map PublishPage (UrlP.s "publish")
        ]

toUrl : Page -> String
toUrl r = 
    case r of
        HomePage -> UrlB.absolute [] [] 
        AppointmentsPage -> UrlB.absolute [ "appointments" ] []
        HostPage -> UrlB.absolute [ "host" ] []
        PublishPage -> UrlB.absolute [ "publish" ] [] 
            
loginUrl : Page -> String
loginUrl r =
    UrlB.absolute [ "login" ] [ UrlB.string "sparoute" (toUrl r) ]

logoutUrl : Page -> String
logoutUrl r = UrlB.absolute [ "logout" ] [ UrlB.string "sparoute" (toUrl r) ] 
    
    
