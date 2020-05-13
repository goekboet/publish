module Hostname exposing(hostnameForm, HostnameForm, initHostnameForm, setHandleValue, setNameValue, setError, addHost, addTimesLink )

import Html exposing (..)
import Html.Attributes exposing (for, type_, method, name, class, classList, disabled)
import Html.Events exposing (onInput, onSubmit)
import Model exposing (Msg(..), Hostname, encodeHostname, decodeHostname)
import Http exposing (Error)
import Url.Builder as UrlB

type alias HandleValue = String
type alias NameValue = String

type ValidationError = BadLength | InvalidChars
type Input = Input (List ValidationError) String 

type alias HostnameForm = 
    { name : Input 
    , handle : Input
    , failed: Maybe Error
    }


isValidInput : Input -> Bool
isValidInput i =
    case i of
        Input [] _ -> True
        _          -> False

isValidHostnameForm : HostnameForm -> Bool
isValidHostnameForm hf = isValidInput hf.name && isValidInput hf.handle
            
hasError : Input -> ValidationError -> Bool 
hasError (Input es _) e = List.any ((==) e) es


handleIsLongerThan2AndShorterThan64 : String -> (Maybe ValidationError) 
handleIsLongerThan2AndShorterThan64 s =
  if String.length s >= 2 && String.length s <= 32
  then Nothing
  else Just BadLength

isValidHandleChar : Char -> Bool
isValidHandleChar c = 
  Char.isLower c || Char.isDigit c || (c == '-') || (c == '_') 

handleHasBadCharacters : HandleValue -> (Maybe ValidationError)
handleHasBadCharacters s =
  if String.all isValidHandleChar s
  then Nothing
  else Just InvalidChars

nameIsLongerThan2AndShorterThan128 : NameValue -> (Maybe ValidationError)
nameIsLongerThan2AndShorterThan128 s =
  if String.length s >= 2 && String.length s <= 64
  then Nothing
  else Just BadLength

pick : List (Maybe a) -> List a
pick = List.filterMap identity

toHandleInput : HandleValue -> Input
toHandleInput s =
    let
        errors = 
            [ handleIsLongerThan2AndShorterThan64 s
            , handleHasBadCharacters s
            ] |> pick
    in
        Input errors s

toNameInput : NameValue -> Input
toNameInput s =
    let
        errors = 
            [ nameIsLongerThan2AndShorterThan128 s ] 
            |> pick
    in
        Input errors s

toHostNameForm : NameValue -> HandleValue -> HostnameForm 
toHostNameForm n h =
    { name = toNameInput n
    , handle = toHandleInput h
    , failed = Nothing}
    

initHostnameForm : HostnameForm
initHostnameForm = toHostNameForm "" ""

setHandleValue : HandleValue -> HostnameForm -> HostnameForm
setHandleValue h hf = { hf | handle = toHandleInput h }

setError : Error -> HostnameForm -> HostnameForm
setError e hf = { hf | failed = Just e }

setNameValue : NameValue -> HostnameForm -> HostnameForm
setNameValue n hf = { hf | name = toNameInput n }

hostDescription : String
hostDescription = String.join " "
    [ "Before you can publish times you need to submit a hostname."
    , "Think of it as an entry in the yellow pages,"
    , "something like Fredrics fortunetelling or Johnson counselling."
    , "This platform is about advertizing some service over videochat."]

handleDescription : String
handleDescription = String.join " "
    [ "The handle is some word that only refers to your host and noone elses."
    , "It is needed for things like making links to your times et.c."
    , "It's normally not very visible to the one you're meeting."
    ]

nameDescription : String
nameDescription = String.join " "
    [ "The name is what you advertize publicly."
    , "When people browse for services this is what they scroll through."
    ]

hostnameStyle : List (Attribute msg)
hostnameStyle =
    [ class "pt-b-05em"
    , class "light-bkg"
    , class "hostNameSubmission"
    , class "main-txt-col" ]



handleInputView : HostnameForm -> Html Msg
handleInputView hf =
    let
        markAsValid i e = 
            if hasError i e 
            then classList [ ("invalid", True) ] 
            else classList [ ("valid", True) ] 
    in
        div [] 
            [ label [ for "handle" ] [ text "handle:" ]
            , input [ type_ "text", name "handle", onInput HandleValueChanged ] [] 
            , ul [] 
                [ li [ markAsValid hf.handle BadLength ] [ text "The handle needs to be between 2 and 64 characters long." ]
                , li [ markAsValid hf.handle InvalidChars ] [ text "Allowed characters in the handle are lower-case characters, digits, _ and -." ]
                ]
            ]

nameInputView : HostnameForm -> Html Msg
nameInputView hf =
    let
        markAsValid i e = 
            if hasError i e 
            then classList [ ("invalid", True) ] 
            else classList [ ("valid", True) ] 
    in
        div [] 
        [ label [ for "name" ] [ text "name:" ]
        , input [ type_ "name", name "name", onInput NameValueChanged ] [] 
        , ul [] 
            [ li [ markAsValid hf.name BadLength ] [ text "The name needs to be between 2 and 128 characters long." ]
            ]
        ]

submitButton : HostnameForm -> Bool -> Html Msg
submitButton hf submitting =
    case (submitting, isValidHostnameForm hf) of
    (True, _) -> button [] [ text "Submitting" ]
    (_, True) -> button [ Html.Events.onClick SubmitHost ] [ text "Submit" ]
    (_, False) -> button [ disabled True ] [ text "Submit" ]

errorView : Error -> Html Msg    
errorView _ =
    p [ class "hostnameSubmissionFail" ] [ text "For some reason we could not register your submission. You're welcome to try again."]

hostnameForm : HostnameForm -> Bool -> Html Msg
hostnameForm hf submitting = Html.div 
    hostnameStyle
    [ h3 [ class "large-text" ] [ text "Submit a hostname"]
    , p [ class "small-text" ] [ text hostDescription]
    , h4 [ class "small-text" ] [ b [] [text "Handle"] ]
    , p [ class "small-text", class "" ] [ text handleDescription ]
    , handleInputView hf
    , h4 [ class "small-text" ] [ b [] [text "Name"] ]
    , p [ class "small-text" ] [ text nameDescription ]
    , nameInputView hf
    , Maybe.map errorView hf.failed |> Maybe.withDefault (text "")
    , submitButton hf submitting
    ]

toHostname : HostnameForm -> Maybe Hostname
toHostname hf =
    case (hf.name, hf.handle) of
        (Input [] n, Input [] h) -> Just { name = n, handle = h}
        _ -> Nothing

addHost : HostnameForm -> Cmd Msg
addHost hf = 
    let
        req h = Http.post
            { url = UrlB.relative [ "api", "publisher" ] []
            , body = Http.jsonBody (encodeHostname h)
            , expect = Http.expectJson HostAdded decodeHostname
            }
    in
        toHostname hf
        |> Maybe.map req
        |> Maybe.withDefault Cmd.none

addTimesText : Hostname -> Html Msg
addTimesText h =
    p [] 
        [ text "Your publisher name is "
        , b [] [ text h.name ] 
        , text "." ]

publishUrl : Hostname -> String
publishUrl h = UrlB.relative [ "publisher", h.handle ] []



addTimesLink : Hostname -> Html Msg
addTimesLink h =
    a [ class "addTimesLink" 
      , Html.Attributes.href (publishUrl h)] 
      [ h3 [] [ text "Publish times" ]
      , addTimesText h ]
    
    
