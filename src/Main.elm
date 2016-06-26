import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html

import Components.Register

-- APP

main : Program Never
main =
  Html.program { init = init, view = view, update = update, subscriptions = subscriptions}


-- MODEL

type Page = HomePage
  | RegisterPage


type alias Model =
  { register : Components.Register.Model
  , currentPage : Page
  }


init : ( Model, Cmd Msg )
init =  (
          { register = Components.Register.initialModel
          ,  currentPage = HomePage
          },
          Cmd.none
        )


-- UPDATE

type Msg = Register Components.Register.Msg


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Register rmsg ->
      let
        registerResult = Components.Register.update rmsg model.register
      in
      ( { model | register = fst registerResult }, Cmd.map Register ( snd registerResult ) )


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.map Register (Components.Register.passwordScore Components.Register.UpdatePasswordScore)


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "container-fluid" ][
    Html.map Register (Components.Register.view model.register)
    , text (toString model)
  ]
