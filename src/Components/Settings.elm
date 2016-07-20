module Components.Settings exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick, onCheck, onInput)
import Html.Attributes exposing (..)
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import HttpBuilder exposing (..)
import Maybe
import Task
import Navigation
import Regex
import Utils.HtmlUtils exposing (..)
import Utils.HttpUtils exposing (httpErrorToString, isTokenExpired)
import Globals


-- MODEL


type alias Model =
    { isTransactionLoggingEnabled : Bool
    , recoveryMethod : RecoveryMethod
    , email : String
    , phrase : List String
    , httpError : Maybe (Error String)
    }


type RecoveryMethod
    = EMail
    | Phrase
    | UnknownRecoveryMethod


recoveryMethodFromString : String -> RecoveryMethod
recoveryMethodFromString str =
    if str == "email" then
        EMail
    else
        Phrase


recoveryMethodToString : RecoveryMethod -> String
recoveryMethodToString method =
    case method of
        EMail ->
            "email"

        Phrase ->
            "phrase"

        UnknownRecoveryMethod ->
            "unknown"


type alias GetSettingsResponse =
    { transactionLogging : Bool
    , recoveryMethod : String
    , email : String
    }


initialModel : Model
initialModel =
    { isTransactionLoggingEnabled = False
    , recoveryMethod = UnknownRecoveryMethod
    , email = ""
    , phrase = []
    , httpError = Nothing
    }


modelToJson : Model -> Globals.Model -> JE.Value
modelToJson model globals =
    JE.object
        [ ( "token", JE.string globals.apiToken )
        , ( "transactionLogging", JE.bool model.isTransactionLoggingEnabled )
        , ( "recoveryMethod", JE.string (recoveryMethodToString model.recoveryMethod) )
        , ( "email", JE.string model.email )
        ]



-- UPDATE


urlChange : Globals.Model -> Cmd Msg
urlChange model =
    if model.apiToken == "" then
        Navigation.newUrl "#login"
    else
        getSettings model


type Msg
    = GetSettingsFailed (Error String)
    | GetSettingSuccessful (Response GetSettingsResponse)
    | PostSettingsFailed (Error String)
    | PostSettingsSuccessful (Response (List String))
    | Save
    | ToggleTransacionLogging Bool
    | ActivateTransactionLogging
    | DeactivateTransactionLogging
    | SetRecoveryMethodEmail
    | SetRecoveryMethodPhrase
    | UpdateEmail String


type alias UpdateResult =
    { model : Model
    , globals : Globals.Model
    , cmd : Cmd Msg
    }


update : Msg -> Model -> Globals.Model -> UpdateResult
update msg model global =
    case msg of
        GetSettingsFailed error ->
            { model = { model | httpError = Just error }
            , globals = global
            , cmd =
                if isTokenExpired error then
                    Navigation.newUrl "#login"
                else
                    Cmd.none
            }

        GetSettingSuccessful result ->
            let
                logging =
                    result.data.transactionLogging

                recovery =
                    recoveryMethodFromString result.data.recoveryMethod

                email =
                    result.data.email
            in
                { model =
                    { model
                        | isTransactionLoggingEnabled = logging
                        , recoveryMethod = recovery
                        , email = email
                    }
                , globals = global
                , cmd = Cmd.none
                }

        PostSettingsFailed error ->
            { model = { model | httpError = Just error }
            , globals = global
            , cmd = Cmd.none
            }

        PostSettingsSuccessful result ->
            { model = { model | phrase = result.data }
            , globals = global
            , cmd = Cmd.none
            }

        Save ->
            { model = model, globals = global, cmd = postSettings model global }

        ToggleTransacionLogging enabled ->
            { model =
                { model
                    | isTransactionLoggingEnabled = enabled
                }
            , globals = global
            , cmd = Cmd.none
            }

        ActivateTransactionLogging ->
            { model = { model | isTransactionLoggingEnabled = True }
            , globals = global
            , cmd = Cmd.none
            }

        DeactivateTransactionLogging ->
            { model = { model | isTransactionLoggingEnabled = False }
            , globals = global
            , cmd = Cmd.none
            }

        SetRecoveryMethodEmail ->
            { model = { model | recoveryMethod = EMail }
            , globals = global
            , cmd = Cmd.none
            }

        SetRecoveryMethodPhrase ->
            { model = { model | recoveryMethod = Phrase }
            , globals = global
            , cmd = Cmd.none
            }

        UpdateEmail mail ->
            { model = { model | email = mail }
            , globals = global
            , cmd = Cmd.none
            }


getSettings : Globals.Model -> Cmd Msg
getSettings global =
    Task.perform
        GetSettingsFailed
        GetSettingSuccessful
        (doGetSettings global)


doGetSettings : Globals.Model -> Task.Task (Error String) (Response GetSettingsResponse)
doGetSettings global =
    let
        baseUrl =
            global.endpoint ++ "/user/settings"

        urlWithParams =
            HttpBuilder.url baseUrl [ ( "token", global.apiToken ) ]

        successReader =
            jsonReader <|
                JD.object3 GetSettingsResponse
                    ("transactionLogging" := JD.bool)
                    ("recoveryMethod" := JD.string)
                    (JD.oneOf [ "email" := JD.string, JD.succeed "" ])

        failReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))
    in
        HttpBuilder.get urlWithParams
            |> send successReader failReader


postSettings : Model -> Globals.Model -> Cmd Msg
postSettings model globals =
    Task.perform
        PostSettingsFailed
        PostSettingsSuccessful
        (doPostSettings model globals)


doPostSettings : Model -> Globals.Model -> Task.Task (Error String) (Response (List String))
doPostSettings model global =
    let
        baseUrl =
            global.endpoint ++ "/user/settings"

        successReader =
            jsonReader (JD.oneOf [ "phrase" := (JD.list JD.string), JD.succeed [] ])

        failReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))

        body =
            modelToJson model global
    in
        HttpBuilder.post baseUrl
            |> withJsonBody body
            |> withHeader "Content-Type" "application/json"
            |> send successReader failReader



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-sm-3" ] []
        , div [ class "col-sm-6" ]
            [ phraseView model, errorView model, formView model ]
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


phraseView : Model -> Html Msg
phraseView model =
    let
        phaseSpans =
            List.map (\str -> span [ class "phrase" ] [ text str ]) model.phrase
    in
        showIfTrue (List.isEmpty model.phrase) <|
            div
                [ class "alert alert-success", attribute "role" "alert" ]
                [ span [ attribute "aria-hidden" "true", class "glyphicon glyphicon-exclamation-sign" ]
                    []
                , text " Your recovery phrase:"
                , div [ class "phrases" ] phaseSpans
                , text "Please save this phrase somewhere, it will not be shown again!"
                ]


formView : Model -> Html Msg
formView model =
    div [ class "panel panel-primary login-form" ]
        [ div [ class "panel-heading" ] [ text "Settings" ]
        , div [ class "panel-body" ]
            [ div [ class <| "form-group" ]
                [ transactionLoggingCheckbox model
                , recoveryMethodDropdown model
                , showIfTrue (model.recoveryMethod == EMail) (emailField model)
                ]
            , button
                [ class "btn btn-primary"
                , type' "submit"
                , onClick Save
                , disabled <| not <| modelValid model
                ]
                [ text "Save" ]
            ]
        ]


transactionLoggingCheckbox : Model -> Html Msg
transactionLoggingCheckbox model =
    div [ class <| "form-group" ]
        [ label [ for "userName" ] [ text "Transaction Logging" ]
        , div []
            [ div [ class "btn-group", attribute "role" "group" ]
                [ button
                    [ class <| "btn btn-default " ++ (toActive <| model.isTransactionLoggingEnabled)
                    , type' "button"
                    , onClick ActivateTransactionLogging
                    ]
                    [ text "Record all transactions on server" ]
                , button
                    [ class <| "btn btn-default " ++ (toActive <| not model.isTransactionLoggingEnabled)
                    , type' "button"
                    , onClick DeactivateTransactionLogging
                    ]
                    [ text "Do not record transactions on server" ]
                ]
            ]
        ]


recoveryMethodDropdown : Model -> Html Msg
recoveryMethodDropdown model =
    div [ class <| "form-group" ]
        [ label [ for "userName" ]
            [ text "Recovery Method" ]
        , div []
            [ div [ class "btn-group", attribute "role" "group" ]
                [ button
                    [ class <| "btn btn-default " ++ (toActive <| model.recoveryMethod == EMail)
                    , type' "button"
                    , onClick SetRecoveryMethodEmail
                    ]
                    [ text "E-Mail" ]
                , button
                    [ class <| "btn btn-default " ++ (toActive <| model.recoveryMethod == Phrase)
                    , type' "button"
                    , onClick SetRecoveryMethodPhrase
                    ]
                    [ text "Recovery Phrase" ]
                ]
            ]
        ]


emailField : Model -> Html Msg
emailField model =
    div [ class <| "form-group" ]
        [ label [ for "userName" ]
            [ text "E-Mail" ]
        , input
            [ class "form-control"
            , placeholder "user@domain.tld"
            , type' "text"
            , value model.email
            , onInput UpdateEmail
            ]
            []
        ]


modelValid : Model -> Bool
modelValid model =
    model.recoveryMethod == Phrase || emailValid model


emailValid : Model -> Bool
emailValid model =
    let
        emailRegex =
            Regex.caseInsensitive (Regex.regex "^\\S+@\\S+\\.\\S+$")
    in
        (Regex.contains emailRegex model.email)
