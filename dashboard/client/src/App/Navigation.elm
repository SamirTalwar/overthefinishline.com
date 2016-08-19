module App.Navigation exposing (..)

import Navigation

import App.Location exposing (Location)

type Message = ShowProjects | HideProjects | NavigateTo Location

type State = ProjectsShown Bool

initialState : State
initialState = ProjectsShown False

update : Message -> State -> (State, Cmd Message)
update message state =
  case (message, state) of
    (ShowProjects, _) -> (ProjectsShown True, Cmd.none)
    (HideProjects, _) -> (ProjectsShown False, Cmd.none)
    (NavigateTo location, state) -> (state, App.Location.navigateTo location)
