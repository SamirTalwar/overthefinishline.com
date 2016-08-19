module App.Location exposing (Location (..), parser)

import Navigation
import UrlParser exposing (..)
import String

type Location = Home | NewProject | Error String

parser : Navigation.Location -> Location
parser location =
  UrlParser.parse identity locationParser (String.dropLeft 1 location.pathname)
    |> resultCase Error identity

locationParser : Parser (Location -> a) a
locationParser =
  oneOf [
    format Home (s ""),
    format NewProject (s "projects" </> s "new")
  ]

resultCase : (a -> c) -> (b -> c) -> Result a b -> c
resultCase errFunction okFunction result =
  case result of
    Ok value -> okFunction value
    Err error -> errFunction error
