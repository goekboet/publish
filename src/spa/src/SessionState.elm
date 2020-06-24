module SessionState exposing (SessionState, isSignedIn, sessionstateView, signinLink, staleSession)

import Html exposing (Html, p, input, text, button, h2, h3, i, div)
import Route exposing (Route, logoutUrl, loginUrl)
import Html.Attributes exposing (class, action, method, type_, name, value, disabled)

type alias SessionState
    = Maybe String

-- init : Maybe String -> SessionState
-- init name = 
--     Maybe.map Fresh name
--     |> Maybe.withDefault None

-- recordStaleness : SessionState
-- recordStaleness = Stale

isSignedIn : SessionState -> Bool
isSignedIn s =
    case s of
        Just _ ->
            True

        _ ->
            False

formLink : Maybe String -> String -> String -> Html msg
formLink csrf url label =
    case csrf of
    Just t -> 
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
                , value t
                ]
                []
            ]
    _ ->
        Html.form
            [ action url
            , method "post"
            , class "formlink"
            ]
            [ button
                [ type_ "submit"
                , disabled True
                ]
                [ text label ]
            ]

logoutTrigger : Route -> Maybe String -> Html msg
logoutTrigger route csrf =
    formLink csrf (logoutUrl route) "Logout"
loginTrigger : Route -> (Maybe String) -> Html msg
loginTrigger route csrf =
    formLink csrf (loginUrl route) "Login"

signinLink : Route -> (Maybe String) -> SessionState -> List (Html msg)
signinLink r csrf ss =
    case ss of
        Nothing -> [ h2 [] [ text "Login required" ]
                , p
                []
                [ text "Publish lets you post times that people can book a videocall with you for. To keep your times apart from everyone elses you need to "
                , loginTrigger r csrf
                , text " so we know who you are."
                ]
            ]
        
            
    
        _ -> []
            
    

sessionstateView : Route -> Maybe String -> SessionState -> Html msg
sessionstateView r csrf s =
    case s of
        Just name ->
            p []
              [ text ("You're logged in as " ++ name)
              , text ". "
              , logoutTrigger r csrf
              ]
            

        _ ->
            p
                [ ]
                [ text "Your session has expired. You need to "
                , loginTrigger r csrf
                , text " again."
                ]
            
staleSession : Route -> Maybe String -> Html msg
staleSession r csrf =
    div [ class "warning" ] 
        [ h3 [] 
            [ i [ class "fas", class "fa-exclamation-triangle" ] []
            , text " Login expired"
            ]
        , p [] 
            [ text "You need to "
            , loginTrigger r csrf
            , text " again."
            ]
        ]
    
    