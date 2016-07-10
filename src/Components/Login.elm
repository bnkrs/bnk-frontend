module Components.Login exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onFocus, onClick)
import Html.Attributes exposing (..)
import String
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import HttpBuilder exposing (..)
import Maybe
import Task
import Navigation

import Globals
import Config


-- MODEL

type alias Model =
  { username : String
  , password : String
  , httpError: Maybe (Error String)
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
  | LoginFailed (Error String)
  | LoginSuccessful (Response String)

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
     , globals = { global | apiToken = token.data }
     , cmd = Navigation.newUrl "#home" }



login : Model -> Cmd Msg
login model =
    Task.perform
      LoginFailed
      LoginSuccessful
      (doLogin model)


doLogin : Model -> Task.Task (Error String) (Response String)
doLogin model =
  let
    loginUrl = Config.rootUrl ++ "/auth/getToken"
    loginSuccessReader = jsonReader ( "token" := JD.string )
    loginFailReader = jsonReader ( JD.at ["error"] ("message" := JD.string ))
    body = modelToJson model
  in
    HttpBuilder.post loginUrl
      |> withJsonBody body
      |> withHeader "Content-Type" "application/json"
      |> send loginSuccessReader loginFailReader





modelToJson : Model -> JE.Value
modelToJson model =
  JE.object
  [ ("username", JE.string model.username)
  , ("password", JE.string model.password) ]


-- view

view : Model -> Html Msg
view model =
  div [ class "row" ][
    div [ class "col-xs-3" ] [],
    div [ class "col-xs-6" ]
      [ errorView model
      ,  formView model
      , a [ href "#register", class "text-center lead"] [text "No account? Register Here!"]
      ],
    div [ class "col-xs-3" ] [ ]
  ]

errorView : Model -> Html Msg
errorView model =
  case model.httpError of
    Just error ->
      div [ class "alert alert-danger", attribute "role" "alert" ]
        [ span [ attribute "aria-hidden" "true", class "glyphicon glyphicon-exclamation-sign" ]
            []
        , span [ class "sr-only" ]
            [ text "Error:" ]
        , text (" " ++ (httpErrorToString error))
        ]
    Nothing ->
      text ""


httpErrorToString : HttpBuilder.Error String -> String
httpErrorToString error =
  case error of
    UnexpectedPayload str ->
      """ Internal Error!
          You can't do anything about this, contact someone who does tech stuff!
      """
    NetworkError ->
      """ Network Error! Make sure you have an internet connection and try again.
          If that does not work, contact someone who administrates your BNK installation!
      """
    Timeout ->
      """ Network Error! Make sure you have an internet connection and try again.
          If that does not work, contact someone who administrates your BNK installation!
      """
    BadResponse response ->
      if response.data ==  "UsernamePasswordWrong"
        then "Wrong username or password!"
        else toString response.data



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
