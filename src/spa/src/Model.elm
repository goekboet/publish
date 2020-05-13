module Model exposing (Msg(..), Hostname, encodeHostname, decodeHostname)

import Browser exposing (UrlRequest)
import Url exposing (Url)
import Json.Decode as Decode exposing (Error, Decoder)
import Json.Encode as Encode exposing (Value)
import Http

type alias Hostname =
    { name : String
    , handle : String
    }

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

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url
  | SubmitHost
  | HostAdded (Result Http.Error Hostname)
  | HandleValueChanged String
  | NameValueChanged String
