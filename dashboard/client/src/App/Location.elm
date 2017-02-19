module App.Location exposing (Location (..), parse, navigateTo, url)

import Maybe
import Navigation
import Url exposing (Url)
import UrlParser exposing (..)

type Location =
    Home
  | Me
  | NewProject
  | EditProject String String
  | Project String String
  | Unknown String

parse : Navigation.Location -> Location
parse location =
  parsePath locationParser location
    |> Maybe.withDefault (Unknown location.pathname)

locationParser : Parser (Location -> a) a
locationParser =
  oneOf [
    map Home (s ""),
    map EditProject (s "projects" </> string </> string </> s "edit"),
    map Project (s "projects" </> string </> string),
    map NewProject (s "projects")
  ]

navigateTo : Location -> Cmd a
navigateTo location = case location of
  Unknown _ -> Cmd.none
  _ -> Navigation.newUrl (Url.toString (url location))

url : Location -> Url
url location =
  Url.withPath <| case location of
    Home -> []
    Me -> ["me"]
    NewProject -> ["projects"]
    EditProject username projectName -> ["projects", username, projectName, "edit"]
    Project username name -> ["projects", username, name]
    Unknown _ -> []
