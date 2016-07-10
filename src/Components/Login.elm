module Components.Login exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onFocus, onClick)
import Html.Attributes exposing (..)
import String
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Http
import Maybe
import Task

import Globals
import Config
import Utils.HttpUtils as HttpUtils


-- MODEL

type alias Model =
  { username : String
  , password : String
  , httpError: Maybe Http.Error
  }


initialModel : Model
initialModel =
  { username = ""
  , password = ""
  , httpError = Nothing
  }


-- UPDATE

type Msg = ChangeUsername String
  | ChangePassword String
  | Login
  | LoginFailed Http.Error
  | LoginSuccessful String

type alias UpdateResult =
  { model : Model
  , globals : Globals.Model
  , cmd: Cmd Msg
  }

update : Msg -> Model -> Globals.Model -> UpdateResult
update msg model global =
  case msg of
    ChangeUsername name ->
      { model = { model | username = name }
      , globals = global
      , cmd = Cmd.none }
    ChangePassword password ->
      { model = { model | password = password }
      , globals = global
      , cmd = Cmd.none }
    Login ->
     { model = model
     , globals = global
     , cmd = login model }
    LoginFailed error ->
     { model = { model | httpError = Just error }
     , globals = global
     , cmd = Cmd.none }
    LoginSuccessful token ->
     { model = model
     , globals = { global | apiToken = token}
     , cmd = Cmd.none }



login : Model -> Cmd Msg
login model =
  let
    loginUrl = Config.rootUrl ++ "/auth/getToken"
    body = Http.string <| modelToJson model
    decoder = "token" := JD.string
  in
    Task.perform
      LoginFailed
      LoginSuccessful
      (HttpUtils.post decoder loginUrl body)


modelToJson : Model -> String
modelToJson model =
  let
    usernamePassword =
      JE.object
      [ ("username", JE.string model.username)
      , ("password", JE.string model.password) ]
  in
    JE.encode 0 usernamePassword


-- view

view : Model -> Html Msg
view model =
  div [ class "row" ][
    div [ class "col-xs-4" ] [],
    div [ class "col-xs-4" ]
      [ formView model
      , a [ href "#register", class "text-center lead"] [text "No account? Register Here!"]
      ],
    div [ class "col-xs-4" ] [ ]
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
          , onClick Login ]
            [ text "Login" ]
        ]
    ]


isValid : Model -> Bool
isValid model = (String.length model.username) > 0 && (String.length model.password > 6)
