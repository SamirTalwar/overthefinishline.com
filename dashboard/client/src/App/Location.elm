module App.Location exposing (Location (..), parser, navigateTo, url)

import Navigation
import String
import Url exposing (Url)
import UrlParser exposing (..)

type Location =
    Home
  | NewProject
  | Project Url
  | Error String

parser : Navigation.Location -> Location
parser location =
  UrlParser.parse identity locationParser (String.dropLeft 1 location.pathname)
    |> resultCase Error identity

locationParser : Parser (Location -> a) a
locationParser =
  oneOf [
    format Home (s ""),
    format (\username projectName -> Project <| Url.withPath ["projects", username, projectName]) (s "projects" </> string </> string),
    format NewProject (s "projects")
  ]

navigateTo : Location -> Cmd a
navigateTo location = case location of
  Error _ -> Cmd.none
  _ -> Navigation.newUrl (Url.toString (url location))

url : Location -> Url
url location = case location of
  Home -> Url.parse "/"
  NewProject -> Url.parse "/projects"
  Project url -> url
  Error _ -> Url.empty

resultCase : (a -> c) -> (b -> c) -> Result a b -> c
resultCase errFunction okFunction result =
  case result of
    Ok value -> okFunction value
    Err error -> errFunction error
