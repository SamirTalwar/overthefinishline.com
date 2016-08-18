module App.Urls exposing (..)

import Url exposing (Url)

root : Url
root = Url.parse "/"

newProject : Url
newProject = Url.parse "/projects/new"
