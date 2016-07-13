port module Components.Login exposing (..)
import Html exposing (..)
import Html.Events exposing (onInput, onFocus, onClick, on, keyCode)
import Html.Attributes exposing (..)
import String
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import HttpBuilder exposing (..)
import Maybe
import Task
import Navigation

import Globals
import Utils.HttpUtils exposing (httpErrorToString)


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

port saveToLocalstorage : Globals.Model -> Cmd msg


-- UPDATE

type Msg = ChangeUsername String
  | ChangePassword String
  | Login
  | LoginFailed (Error String)
  | LoginSuccessful (Response String)
  | NoOp

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
     , cmd = login model global }
    LoginFailed error ->
     { model = { model | httpError = Just error, password = "" }
     , globals = global
     , cmd = Cmd.none }
    LoginSuccessful token ->
      let
        newGlobals = { global | apiToken = token.data , username = model.username }
      in
       { model = { model | password = "" }
       , globals = newGlobals
       , cmd = Cmd.batch [saveToLocalstorage newGlobals, Navigation.newUrl "#home"] }
    NoOp ->
     { model = model
     , globals = global
     , cmd = Cmd.none }




login : Model -> Globals.Model -> Cmd Msg
login model global =
    Task.perform
      LoginFailed
      LoginSuccessful
      (doLogin model global)


doLogin : Model -> Globals.Model -> Task.Task (Error String) (Response String)
doLogin model global =
  let
    loginUrl = global.endpoint ++ "/auth/getToken"
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


-- VIEW

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


formView : Model -> Html Msg
formView model =
  div [class "panel panel-primary login-form"]
    [
      div [class "panel-heading"] [text "Login"],
      div [class "panel-body"]
        [ div [ class <| "form-group"]
            [ label [ for "userName" ]
                [ text "Username" ]
            , input [ class "form-control"
              , placeholder "Username", type' "text", onInput ChangeUsername ]
                []
            ]
        , div [ class <| "form-group"]
            [ label [ for "password" ]
                [ text "Password" ]
            , input [ class "form-control"
              , placeholder "Password", type' "password"
              , onInput ChangePassword
              , onEnter Login ]
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

onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    tagger code =
      if code == 13 then msg else NoOp
  in
    on "keydown" (JD.map tagger keyCode)
