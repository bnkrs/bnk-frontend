import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Html.Events exposing ( onClick )

import Components.Register


-- APP
main : Program Never
main =
  Html.beginnerProgram { model = model, view = view, update = update }


-- MODEL
type alias Model = { register : Components.Register.Model}

model : Model
model = { register = Components.Register.initialModel}


-- UPDATE
type Msg = NoOp | Register Components.Register.Msg

update : Msg -> Model -> Model
update msg model =
  case msg of
    NoOp -> model
    Register rmsg ->
      { model | register = Components.Register.update rmsg model.register }


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container-fluid" ][    -- inline CSS (literal)
    div [ class "row" ][
      div [ class "col-xs-12" ][

      ]
    ]
  ]
