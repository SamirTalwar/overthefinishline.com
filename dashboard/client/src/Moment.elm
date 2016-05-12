module Moment exposing (
    Moment,
    Duration,
    DurationUnit (..),
    now,
    parse,
    format,
    compare,
    durationOf,
    durationBetween,
    from,
    decode
  )

import Json.Decode exposing (Decoder, customDecoder, string)
import Result exposing (Result (..))
import Task exposing (Task)

import Native.Moment

type Moment = Moment

type alias Duration = Int

type DurationUnit = Hours

now : () -> Task never Moment
now _ = Native.Moment.now ()

parse : String -> Result String Moment
parse = Native.Moment.parse

format : Moment -> String
format = Native.Moment.format

compare : Moment -> Moment -> Order
compare = Native.Moment.compare

durationOf : Int -> DurationUnit -> Duration
durationOf value unit =
  case unit of
    Hours -> Native.Moment.durationOf value "hours"

durationBetween : Moment -> Moment -> Duration
durationBetween = Native.Moment.durationBetween

from : Moment -> Moment -> String
from = Native.Moment.from

decode : Decoder Moment
decode = customDecoder string parse
