module Model exposing (Msg(..))

import Browser exposing (UrlRequest)
import Url exposing (Url)

type Msg
  = LinkClicked UrlRequest
  | UrlChanged Url