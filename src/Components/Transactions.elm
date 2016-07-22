module Components.Transactions exposing (..)

import Globals
import Navigation
import HttpBuilder exposing (..)
import Task
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Html exposing (..)
import Html.Attributes exposing (..)
import String
import Result
import Navigation
import Utils.HttpUtils exposing (httpErrorToString, isTokenExpired)
import Utils.HtmlUtils exposing (..)


-- MODEL


type alias Model =
    { receiver : String
    , value : Int
    , httpError : Maybe (Error String)
    , success : Bool
    }


initialModel : Model
initialModel =
    { receiver = ""
    , value = 0
    , httpError = Nothing
    , success = False
    }


modelToJson : Model -> String -> JE.Value
modelToJson { receiver, value } apiToken =
    JE.object
        [ ( "token", JE.string apiToken )
        , ( "receiver", JE.string receiver )
        , ( "value", JE.int value )
        ]



-- UPDATE


type Msg
    = UpdateReceiver String
    | UpdateValue String
    | TransactionReqeustFailed (Error String)
    | TransactionReqeustSuccessFull (Response Bool)
    | Send


type alias UpdateResult =
    { model : Model
    , globals : Globals.Model
    , cmd : Cmd Msg
    }


update : Msg -> Model -> Globals.Model -> UpdateResult
update msg model globals =
    case msg of
        UpdateReceiver rec ->
            UpdateResult { model | receiver = rec } globals Cmd.none

        UpdateValue str ->
            let
                val =
                    (Result.withDefault 0 (String.toInt str)) * 100
            in
                UpdateResult { model | value = val } globals Cmd.none

        TransactionReqeustFailed error ->
            if isTokenExpired error then
                UpdateResult { model | httpError = Just error } (Globals.logout globals) (Navigation.newUrl "#login")
            else
                UpdateResult { model | httpError = Just error } globals Cmd.none

        TransactionReqeustSuccessFull result ->
            UpdateResult { model | success = True } globals Cmd.none

        Send ->
            UpdateResult model globals (requestBalance model globals)


requestBalance : Model -> Globals.Model -> Cmd Msg
requestBalance model globals =
    Task.perform
        TransactionReqeustFailed
        TransactionReqeustSuccessFull
        (doBalanceRequest model globals)


doBalanceRequest : Model -> Globals.Model -> Task.Task (Error String) (Response Bool)
doBalanceRequest model { endpoint, apiToken } =
    let
        successReader =
            jsonReader ("success" := JD.bool)

        failReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))

        body =
            modelToJson model apiToken
    in
        HttpBuilder.post (endpoint ++ "/user/send")
            |> withJsonBody body
            |> withHeader "Content-Type" "application/json"
            |> send successReader failReader



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-xs-1" ] []
        , div [ class "col-xs-8" ]
            [ errorView model
            , successView model.success
            , formView model
            ]
        , div [ class "col-xs-1" ] []
        ]


formView : Model -> Html Msg
formView model =
    div [ class "panel panel-primary login-form" ]
        [ div [ class "panel-heading" ] [ text "Make Transaction" ]
        , div [ class "panel-body" ]
            [ textFieldWithLabel UpdateReceiver "Payment Recipient" "Payment Recipient"
            , inputFieldWithLabel
                { inputHandler = UpdateValue
                , focusHandler = Nothing
                , enterHandler = Nothing
                , inputType = "number"
                , labelText = "Money To Send (in €)"
                , placeholderText = "0"
                , hasError = False
                , inputValue = Nothing
                }
            , primaryButton Send (isValid model) "Send"
            ]
        ]


isValid : Model -> Bool
isValid model =
    model.value > 0 && not (String.isEmpty model.receiver)


errorView : Model -> Html Msg
errorView model =
    case model.httpError of
        Just error ->
            Utils.HtmlUtils.errorView (httpErrorToString error)

        Nothing ->
            text ""


successView : Bool -> Html Msg
successView successfull =
    showIfTrue successfull <|
        div
            [ class "alert alert-success", attribute "role" "alert" ]
            [ span [ attribute "aria-hidden" "true", class "glyphicon glyphicon-exclamation-sign" ]
                []
            , text " Your money was successfully transfered."
            ]
