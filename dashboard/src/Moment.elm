module Moment (
    Moment,
    parse,
    format,
    compare,
    decode
  ) where

import Native.Moment

import Json.Decode exposing (Decoder, customDecoder, string)
import Result exposing (Result (..))

type Moment = Moment

parse : String -> Result String Moment
parse = Native.Moment.parse

format : Moment -> String
format = Native.Moment.format

compare : Moment -> Moment -> Order
compare = Native.Moment.compare

decode : Decoder Moment
decode = customDecoder string parse
