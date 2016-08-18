module App.Navigation exposing (..)

import Navigation
import Url exposing (Url)

type Message = ShowProjects | HideProjects | NavigateTo Url

type State = ProjectsShown Bool

initialState : State
initialState = ProjectsShown False

update : Message -> State -> (State, Cmd Message)
update message state =
  case (message, state) of
    (ShowProjects, _) -> (ProjectsShown True, Cmd.none)
    (HideProjects, _) -> (ProjectsShown False, Cmd.none)
    (NavigateTo url, state) -> (state, Navigation.newUrl (Url.toString url))
