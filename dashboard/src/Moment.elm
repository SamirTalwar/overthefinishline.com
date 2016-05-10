module Moment (
    Moment,
    now,
    parse,
    format,
    compare,
    durationBetween,
    from,
    decode
  ) where

import Native.Moment

import Json.Decode exposing (Decoder, customDecoder, string)
import Result exposing (Result (..))
import Task exposing (Task)

type Moment = Moment

now : () -> Task never Moment
now _ = Native.Moment.now ()

parse : String -> Result String Moment
parse = Native.Moment.parse

format : Moment -> String
format = Native.Moment.format

compare : Moment -> Moment -> Order
compare = Native.Moment.compare

durationBetween : Moment -> Moment -> Int
durationBetween = Native.Moment.durationBetween

from : Moment -> Moment -> String
from = Native.Moment.from

decode : Decoder Moment
decode = customDecoder string parse
