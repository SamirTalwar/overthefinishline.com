module Navigation exposing (..)

type alias Message = State

type State = ProjectsShown Bool

initialState : State
initialState = ProjectsShown False

state : Message -> State
state = identity
