module Moment (
    Moment,
    parse,
    format,
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

decode : Decoder Moment
decode = customDecoder string parse
