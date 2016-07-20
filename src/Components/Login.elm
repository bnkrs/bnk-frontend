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
import Utils.HtmlUtils exposing (..)


-- MODEL


type alias Model =
    { username : String
    , password : String
    , httpError : Maybe (Error String)
    }


initialModel : Model
initialModel =
    { username = ""
    , password = ""
    , httpError = Nothing
    }


port saveToLocalstorage : Globals.Model -> Cmd msg



-- UPDATE


type Msg
    = ChangeUsername String
    | ChangePassword String
    | Login
    | LoginFailed (Error String)
    | LoginSuccessful (Response String)
    | NoOp


type alias UpdateResult =
    { model : Model
    , globals : Globals.Model
    , cmd : Cmd Msg
    }


update : Msg -> Model -> Globals.Model -> UpdateResult
update msg model globals =
    case msg of
        ChangeUsername name ->
            UpdateResult { model | username = name } globals Cmd.none

        ChangePassword password ->
            UpdateResult { model | password = password } globals Cmd.none

        Login ->
            UpdateResult model globals (login model globals)

        LoginFailed error ->
            UpdateResult { model | httpError = Just error, password = "" } globals Cmd.none

        LoginSuccessful token ->
            let
                newGlobals =
                    { globals | apiToken = token.data, username = model.username }
            in
                UpdateResult
                    { model | password = "" }
                    newGlobals
                    (Cmd.batch [ saveToLocalstorage newGlobals, Navigation.newUrl "#home" ])

        NoOp ->
            UpdateResult model globals Cmd.none


login : Model -> Globals.Model -> Cmd Msg
login model global =
    Task.perform
        LoginFailed
        LoginSuccessful
        (doLogin model global)


doLogin : Model -> Globals.Model -> Task.Task (Error String) (Response String)
doLogin model global =
    let
        loginUrl =
            global.endpoint ++ "/auth/getToken"

        loginSuccessReader =
            jsonReader ("token" := JD.string)

        loginFailReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))

        body =
            modelToJson model
    in
        HttpBuilder.post loginUrl
            |> withJsonBody body
            |> withHeader "Content-Type" "application/json"
            |> send loginSuccessReader loginFailReader


modelToJson : Model -> JE.Value
modelToJson model =
    JE.object
        [ ( "username", JE.string model.username )
        , ( "password", JE.string model.password )
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-sm-3" ] []
        , div [ class "col-sm-6" ]
            [ errorView model
            , formView model
            , a [ href "#register", class "text-center lead" ] [ text "No account? Register Here!" ]
            ]
        , div [ class "col-sm-3" ] []
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
    div [ class "panel panel-primary login-form" ]
        [ div [ class "panel-heading" ] [ text "Login" ]
        , div [ class "panel-body" ]
            [ textFieldWithLabel ChangeUsername "Username" "Username"
            , inputFieldWithLabel
                { inputHandler = ChangePassword
                , focusHandler = Nothing
                , enterHandler = Just ( Login, NoOp )
                , inputType = "password"
                , labelText = "Password"
                , placeholderText = "Password"
                , hasError = False
                , inputValue = Nothing
                }
            , button
                [ class "btn btn-primary"
                , type' "submit"
                , disabled <| not (isValid model)
                , onClick Login
                ]
                [ text "Login" ]
            ]
        ]


isValid : Model -> Bool
isValid model =
    (String.length model.username) > 0 && (String.length model.password > 6)
