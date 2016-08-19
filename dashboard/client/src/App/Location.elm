module App.Location exposing (Location (..), parser, navigateTo, url)

import Html exposing (Attribute)
import Html.Attributes
import Navigation
import String
import Url exposing (Url)
import UrlParser exposing (..)

type Location =
    Home
  | NewProject
  | Project String
  | Error String

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

navigateTo : Location -> Cmd a
navigateTo location = case location of
  Error _ -> Cmd.none
  _ -> Navigation.newUrl (Url.toString (url location))

url : Location -> Url
url location = case location of
  Home -> Url.parse "/"
  NewProject -> Url.parse "/projects/new"
  Project url -> Url.parse url
  Error _ -> Url.empty

resultCase : (a -> c) -> (b -> c) -> Result a b -> c
resultCase errFunction okFunction result =
  case result of
    Ok value -> okFunction value
    Err error -> errFunction error
