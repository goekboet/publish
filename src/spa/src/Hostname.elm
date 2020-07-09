module Hostname exposing
    ( Model(..)
    , hasHostname
    , init
    , Msg(..)
    , update
    , view
    )

import Html exposing (..)
import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder)
import Html.Attributes exposing (for, type_, name, class, classList, disabled)
import Html.Events exposing (onInput)
import Http exposing (Error)
import Url.Builder as UrlB

type ValidationError = BadLength | InvalidChars
type Input = Input (List ValidationError) String 

type alias HostnameForm = 
    { name : Input 
    , handle : Input
    , failed: Maybe Error
    }

type alias Hostname =
    { name : String
    , handle : String
    }

type Model =
  Unsubmitted HostnameForm
  | Submitting HostnameForm
  | FailedSubmit HostnameForm
  | Submitted Hostname

hasHostname : Model -> Maybe Hostname
hasHostname m =
  case m of
    Submitted hn -> Just hn
    _            -> Nothing  

init : Maybe String -> Maybe String -> Model
init name handle =
  Maybe.map2 Hostname name handle
  |> Maybe.map Submitted
  |> Maybe.withDefault (Unsubmitted initHostnameForm)

encodeHostname : Hostname -> Value
encodeHostname h =
    Encode.object 
        [ ("name", Encode.string h.name )
        , ("handle", Encode.string h.handle )
        ]

decodeHostname : Decoder Hostname
decodeHostname =
    Decode.map2 Hostname
        (Decode.field "name" Decode.string)
        (Decode.field "handle" Decode.string)

type alias HandleValue = String
type alias NameValue = String

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

isAuthError : Http.Error -> Bool
isAuthError e =
  case e of
    Http.BadStatus 401 -> True
    _ -> False

type Msg
    = HandleValueChanged String
    | NameValueChanged String
    | SubmitHost
    | HostAdded (Result Http.Error Hostname)

update : (Msg -> msg) -> Model -> Msg -> Maybe (Model, Cmd msg) 
update toAppMsg model msg = 
    case msg of
        HostAdded (Ok h) -> 
          Just 
          ( Submitted h 
          , Cmd.none
          )

        HostAdded (Err e) ->
          if isAuthError e
          then Nothing 
          else case model of
                Submitting hf -> Just ( FailedSubmit (hf |> setError e), Cmd.none)
                _             -> Just ( FailedSubmit (initHostnameForm |> setError e), Cmd.none )
        HandleValueChanged s -> 
          case model of
            Unsubmitted hf -> Just ( Unsubmitted (setHandleValue s hf), Cmd.none)
            _              -> Just ( model, Cmd.none )
    
        NameValueChanged s -> 
          case model of
            Unsubmitted hf -> Just ( Unsubmitted (setNameValue s hf), Cmd.none)
            _              -> Just ( model, Cmd.none )
    
        SubmitHost -> 
          case model of
            Unsubmitted hf -> Just ( Submitting hf, addHost (toAppMsg << HostAdded) hf)
            _              -> Just (model, Cmd.none )

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

handleInputView : (String -> msg) -> HostnameForm -> Html msg
handleInputView valuechanged hf =
    let
        pass i e = 
            if hasError i e 
            then classList [("pass", False)] 
            else classList [("pass", True) ]  
    in
        div [ class "forminput" ] 
            [ label [ for "handle" ] [ text "handle:" ]
            , input [ type_ "text", name "handle", onInput valuechanged ] [] 
            , span [ pass hf.handle BadLength ] [ text "The handle needs to be between 2 and 64 characters long." ]
            , span [ pass hf.handle InvalidChars ] [ text "Allowed characters in the handle are lower-case characters, digits, _ and -." ]
            ]

nameInputView : (String -> msg) -> HostnameForm -> Html msg
nameInputView valuechanged hf =
    let
        pass i e = 
            if hasError i e 
            then classList [("pass", False)] 
            else classList [("pass", True) ] 
    in
        div [ class "forminput" ] 
        [ label [ for "name" ] [ text "name:" ]
        , input [ type_ "text", name "name", onInput valuechanged ] [] 
        , span [ pass hf.name BadLength ] [ text "The name needs to be between 2 and 128 characters long." ]
        ]

submitButton : msg -> HostnameForm -> Bool -> Html msg
submitButton submit hf submitting =
    case (submitting, isValidHostnameForm hf) of
    (True, _) -> div [class "formbuttons"] [button [ disabled True ] [ text "Submitting" ]]
    (_, True) -> div [class "formbuttons"] [button [ Html.Events.onClick submit ] [ text "Submit" ]]
    (_, False) -> div [class "formbuttons"] [button [ disabled True ] [ text "Submit" ]]

errorView : Error -> Html msg    
errorView _ =
    p [ class "hostnameSubmissionFail" ] [ text "For some reason we could not register your submission. You're welcome to try again."]

hostnameForm : (String -> msg) -> (String -> msg) -> msg -> HostnameForm -> Bool -> Html msg
hostnameForm namechange handlechange submit hf submitting = Html.div 
    [ ]
    [ h2 [] [ text "Submit a hostname"]
    , p [] [ text hostDescription]
    , h3 [] [ b [] [text "Handle"] ]
    , p [] [ text handleDescription ]
    , handleInputView handlechange hf
    , h3 [] [ b [] [text "Name"] ]
    , p [] [ text nameDescription ]
    , nameInputView namechange hf
    , Maybe.map errorView hf.failed |> Maybe.withDefault (text "")
    , submitButton submit hf submitting
    ]

toHostname : HostnameForm -> Maybe Hostname
toHostname hf =
    case (hf.name, hf.handle) of
        (Input [] n, Input [] h) -> Just { name = n, handle = h}
        _ -> Nothing

addHost : (Result Error Hostname -> msg) -> HostnameForm -> Cmd msg
addHost response hf = 
    let
        req h = Http.post
            { url = UrlB.relative [ "api", "publisher" ] []
            , body = Http.jsonBody (encodeHostname h)
            , expect = Http.expectJson response decodeHostname
            }
    in
        toHostname hf
        |> Maybe.map req
        |> Maybe.withDefault Cmd.none 

view : (Msg -> msg) -> Model -> List (Html msg)
view toAppMsg m =
    case m of
      Unsubmitted hnf -> [ hostnameForm (toAppMsg << NameValueChanged) (toAppMsg << HandleValueChanged) (toAppMsg SubmitHost) hnf False ]
      Submitting hnf -> [ hostnameForm (toAppMsg << NameValueChanged) (toAppMsg << HandleValueChanged) (toAppMsg SubmitHost) hnf True ]
      FailedSubmit hnf -> [ hostnameForm (toAppMsg << NameValueChanged) (toAppMsg << HandleValueChanged) (toAppMsg SubmitHost) hnf False ]
      Submitted hn -> 
        [ h2 [] [ text "Hostname" ]
        , p [] 
          [ text ("You've registered " ++ hn.name ++ " as hostname. Go on and ")
          , a 
            [ Html.Attributes.href "/publish" ]
            [ text "publish" ]
          , text " some times already."
          ] 
        ]
      