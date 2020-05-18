module SessionState exposing (SessionState(..), init, recordStaleness, isSignedIn, sessionstateView, signinLink)

import Html exposing (Html, p, input, text, button, h2)
import Route exposing (Route, logoutUrl, loginUrl)
import Html.Attributes exposing (class, action, method, type_, name, value)

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

formLink : String -> String -> String -> Html msg
formLink csrf url label =
    Html.form
        [ action url
        , method "post"
        , class "formlink"
        ]
        [ button
            [ type_ "submit"
            ]
            [ text label ]
        , input
            [ type_ "hidden"
            , name "__RequestVerificationToken"
            , value csrf
            ]
            []
        ]

logoutTrigger : Route -> String -> Html msg
logoutTrigger route csrf =
    formLink csrf (logoutUrl route) "Logout"
loginTrigger : Route -> String -> Html msg
loginTrigger route csrf =
    formLink csrf (loginUrl route) "Login"

signinLink : Route -> String -> SessionState -> List (Html msg)
signinLink r csrf ss =
    case ss of
        None -> [ h2 [] [ text "Login required" ]
                , p
                []
                [ text "Publish lets you post times that people can book a videocall with you for. To keep your times apart from everyone elses you need to "
                , loginTrigger r csrf
                , text " so we know who you are."
                ]
            ]
        
            
    
        _ -> []
            
    

sessionstateView : Route -> String -> SessionState -> Html msg
sessionstateView r csrf s =
    case s of
        Fresh name ->
            p []
              [ text ("You're logged in as " ++ name)
              , text ". "
              , logoutTrigger r csrf
              ]
            

        Stale ->
            p
                [ ]
                [ text "Your session has expired. You need to "
                , loginTrigger r csrf
                , text " again."
                ]
            
        None -> text ""