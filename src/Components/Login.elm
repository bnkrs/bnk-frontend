module Components.Login exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onFocus, onClick)
import Html.Attributes exposing (..)
import String


-- MODEL

type alias Model =
  { username : String
  , password : String
  }


initialModel : Model
initialModel =
  { username = ""
  , password = ""
  }


-- UPDATE

type Msg = ChangeUsername String
  | ChangePassword String
  | Login


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeUsername name ->
      ( { model | username = name }, Cmd.none )
    ChangePassword password ->
      ( { model | password = password }, Cmd.none )
    Login ->
      ( model, Cmd.none)


-- view

view : Model -> Html Msg
view model =
  div [ class "row" ][
    div [ class "col-xs-4" ] [],
    div [ class "col-xs-4" ][ formView model],
    div [ class "col-xs-4" ] []
  ]


formView : Model -> Html Msg
formView model =
  div [class "panel panel-primary login-form"]
    [
      div [class "panel-heading"] [text "Login"],
      div [class "panel-body"]
        [ div [ class <| "form-group"]
            [ label [ for "userName" ]
                [ text "Username" ]
            , input [ class "form-control", id "userName"
              , placeholder "Username", type' "text", onInput ChangeUsername ]
                []
            ]
        , div [ class <| "form-group"]
            [ label [ for "password" ]
                [ text "Password" ]
            , input [ class "form-control", id "password"
              , placeholder "Password", type' "password"
              , onInput ChangePassword ]
                []
            ]
        , button [ class "btn btn-primary", type' "submit"
          , disabled <| not (isValid model)
          , onClick Login]
            [ text "Login" ]
        ]
    ]


isValid : Model -> Bool
isValid model = (String.length model.username) > 0 && (String.length model.password > 6)
