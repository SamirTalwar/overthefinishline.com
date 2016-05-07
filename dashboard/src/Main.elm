import StartApp.Simple exposing (start)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

model = 0

type Action = Increment | Decrement

update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1

view address model =
  div []
    [
      button [onClick address Decrement] [text "-"],
      div [] [text (toString model)],
      button [onClick address Increment] [text "+"]
    ]

main =
  start {
    model = model,
    update = update,
    view = view
  }
