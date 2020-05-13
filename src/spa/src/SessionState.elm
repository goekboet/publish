module SessionState exposing (SessionState(..), init, recordStaleness, isSignedIn, sessionstateView, sessionStateStyle, signinLink)

import Html exposing (Html, Attribute, p, i, input, text, b, button, a, div, h3)
import Route exposing (Route, logoutUrl, loginUrl)
import Html.Attributes exposing (class, action, method, type_, name, value)
import Html.Events exposing (onClick)
import Model exposing (Msg(..))

type SessionState
    = Fresh String
    | Stale
    | None

init : Maybe String -> SessionState
init name = 
    Maybe.map Fresh name
    |> Maybe.withDefault None

recordStaleness : SessionState
recordStaleness = Stale

isSignedIn : SessionState -> Bool
isSignedIn s =
    case s of
        Fresh _ ->
            True

        _ ->
            False
sessionStateText : List (Attribute Msg)
sessionStateText =
    [ class "alt-txt-col"
    , class "small-text"
    , class "inline"
    ]

logoutTrigger : Route -> String -> Html Msg
logoutTrigger route csrf =
    Html.form
        [ action (logoutUrl route)
        , method "post"
        , class "inline"
        , class "logoutTrigger"
        ]
        [ button
            [ type_ "submit"
            , class "alt-txt-col"
            ]
            [ text "log out" ]
        , input
            [ type_ "hidden"
            , name "__RequestVerificationToken"
            , value csrf
            ]
            []
        ]

loginTrigger : Route -> String -> Html Msg
loginTrigger route csrf =
    Html.form
        [ action (loginUrl route)
        , method "post"
        , class "inline"
        , class "logoutTrigger"
        ]
        [ button
            [ type_ "submit"
            , class "main-txt-col"
            ]
            [ text "login" ]
        , input
            [ type_ "hidden"
            , name "__RequestVerificationToken"
            , value csrf
            ]
            []
        ]

sessionStateStyle : List (Attribute Msg)
sessionStateStyle =
    [ class "pb-1em"
    , class "heavy-bkg"
    ]

signinLink : Route -> String -> SessionState -> Html Msg
signinLink r csrf ss =
    case ss of
        None -> div [ class "notFoundView" ] 
            [ h3 [] [ text "Login required" ]
            , p
                []
                [ text "Publish lets you post times that people can book a videocall with you for. To keep your times apart from everyone elses you need to "
                , loginTrigger r csrf
                , text " so we know who you are."
                ]
            ]
        
            
    
        _ -> text ""
            
    

sessionstateView : Route -> String -> SessionState -> List (Html Msg)
sessionstateView r csrf s =
    case s of
        Fresh name ->
            [ p
                sessionStateText
                [ text "You are logged in as " ]
            , b sessionStateText [ text name ]
            , text "."
            , logoutTrigger r csrf
            ]

        Stale ->
            [ p
                [ class "alt-txt-col"
                , class "small-text"
                ]
                [ text "Your session has expired. You need to "
                , loginTrigger r csrf
                , text " again."
                ]
            ]
        None -> []