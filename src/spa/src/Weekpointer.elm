port module Weekpointer exposing 
    ( Weekpointer
    , Week
    , Hour
    , Day
    , TsLookup
    , getWeekWindow
    , moveWeekpointer
    , newWeekpointer
    )

type alias Hour =
    { name : String
    , ts: Int
    , isNow : Bool
    }

type alias Day = 
    { key: String
    , name: String
    , start: Int
    , end : Int
    , isNow : Bool
    , hours: List Hour
    }

type alias TsLookup =
    { week : Weekpointer
    , days : List Day
    }

type alias Week = 
    { name : String
    , ts: Int
    , isNow : Bool
    }

type alias Weekpointer =
    { current: Week
    , previous: Week
    , next : Week
    }

getWeekWindow : Weekpointer -> (Int, Int)
getWeekWindow { current, previous, next } =
    (current.ts, next.ts)

port moveWeekpointer : Maybe Int -> Cmd msg
port newWeekpointer : (TsLookup -> msg) -> Sub msg