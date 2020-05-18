port module Weekpointer exposing (WeekPointer)

import Json.Encode as Encode exposing (Value)
import Json.Decode as Decode exposing (Decoder, field, string)

port getWeekpointer : Value -> Cmd a
port gotWeekpointer : (Value -> msg) -> Sub msg

type alias Week =
    String

type alias Start =
    Int

type alias End =
    Int
type alias Window = ( Start, End )

type alias WeekPointer =
    { prev : Week
    , curr : Week
    , next : Week
    , window : Window
    }

encodeWeek : Week -> Value
encodeWeek w =
    Encode.string w

decodeTimesWindow : Decoder Window
decodeTimesWindow =
    Decode.map2 Tuple.pair (Decode.index 0 Decode.int) (Decode.index 1 Decode.int)


decodeWeekpointer : Decoder WeekPointer
decodeWeekpointer =
    Decode.map4
        WeekPointer
        (Decode.field "prev" Decode.string)
        (Decode.field "curr" Decode.string)
        (Decode.field "next" Decode.string)
        (Decode.field "window" decodeTimesWindow)

