module Components.Home exposing (..)

import Globals
import Navigation
import HttpBuilder exposing (..)
import Task
import Json.Decode as JD exposing ((:=))
import Html exposing (..)
import Html.Attributes exposing (..)
import Date
import String
import Navigation
import Utils.HttpUtils exposing (httpErrorToString, isTokenExpired)
import Utils.HtmlUtils exposing (..)


-- MODEL


type alias Model =
    { balance : Maybe Int
    , httpError : Maybe (Error String)
    , transactions : List Transaction
    }


type alias Transaction =
    { sender : String
    , receiver : String
    , value : Int
    , timestamp : Int
    }


initialModel : Model
initialModel =
    { balance = Nothing
    , httpError = Nothing
    , transactions = []
    }



-- UPDATE


type Msg
    = RequestBalance
    | RequestFailed (Error String)
    | BalanceRequestSuccessfull (Response Int)
    | TransactionsRequestSuccessfull (Response (List Transaction))


type alias UpdateResult =
    { model : Model
    , globals : Globals.Model
    , cmd : Cmd Msg
    }


update : Msg -> Model -> Globals.Model -> UpdateResult
update msg model globals =
    case msg of
        RequestBalance ->
            UpdateResult model globals (requestBalance globals)

        RequestFailed error ->
            if isTokenExpired error then
                UpdateResult { model | httpError = Just error } (Globals.logout globals) (Navigation.newUrl "#login")
            else
                UpdateResult { model | httpError = Just error } globals Cmd.none

        BalanceRequestSuccessfull result ->
            UpdateResult { model | balance = Just result.data } globals Cmd.none

        TransactionsRequestSuccessfull result ->
            UpdateResult { model | transactions = List.reverse result.data } globals Cmd.none


urlChange : Globals.Model -> Cmd Msg
urlChange globals =
    if globals.apiToken == "" then
        Navigation.newUrl "#login"
    else
        Cmd.batch [ (requestBalance globals), (requestTransaction globals) ]


requestTransaction : Globals.Model -> Cmd Msg
requestTransaction global =
    Task.perform
        RequestFailed
        TransactionsRequestSuccessfull
        (doTransactionRequest global)


doTransactionRequest : Globals.Model -> Task.Task (Error String) (Response (List Transaction))
doTransactionRequest { endpoint, apiToken } =
    let
        baseUrl =
            endpoint ++ "/user/transactions"

        tranasctionUrl =
            HttpBuilder.url baseUrl [ ( "token", apiToken ) ]

        successReader =
            jsonReader <|
                JD.at [ "transactions" ] <|
                    JD.list <|
                        JD.object4
                            Transaction
                            ("sender" := JD.string)
                            ("receiver" := JD.string)
                            ("value" := JD.int)
                            ("timestamp" := JD.int)

        failReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))
    in
        HttpBuilder.get tranasctionUrl
            |> withHeader "Content-Type" "application/json"
            |> send successReader failReader


requestBalance : Globals.Model -> Cmd Msg
requestBalance global =
    Task.perform
        RequestFailed
        BalanceRequestSuccessfull
        (doBalanceRequest global)


doBalanceRequest : Globals.Model -> Task.Task (Error String) (Response Int)
doBalanceRequest global =
    let
        baseUrl =
            global.endpoint ++ "/user/balance"

        balanceUrl =
            HttpBuilder.url baseUrl [ ( "token", global.apiToken ) ]

        successReader =
            jsonReader ("balance" := JD.int)

        failReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))
    in
        HttpBuilder.get balanceUrl
            |> withHeader "Content-Type" "application/json"
            |> send successReader failReader



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-xs-1" ] []
        , div [ class "col-xs-8" ]
            [ errorView model
            , div
                [ class "lead" ]
                [ text "Current Balance: "
                , text <| formatBalance model.balance
                ]
            , transactionTable model.transactions
            ]
        , div [ class "col-xs-1" ] []
        ]


formatBalance : Maybe Int -> String
formatBalance balance =
    case balance of
        Just value ->
            (toString <| (toFloat value) / 100) ++ " " ++ "€"

        Nothing ->
            "No data yet"


transactionRow : Transaction -> Html Msg
transactionRow { sender, receiver, value, timestamp } =
    let
        date =
            timestamp
                |> toFloat
                |> Date.fromTime
                |> toString
    in
        tr []
            [ td [] [ text <| String.slice 1 ((String.length date) - 1) date ]
            , td [] [ text sender ]
            , td [] [ text receiver ]
            , td [] [ text <| toString (toFloat value / 100) ++ " €" ]
            ]


transactionTable : List Transaction -> Html Msg
transactionTable transactions =
    table [ class "table table-striped" ]
        [ thead []
            [ th [] [ text "Date" ]
            , th [] [ text "Sender" ]
            , th [] [ text "Recipient" ]
            , th [] [ text "Amount" ]
            ]
        , tbody []
            (List.map transactionRow transactions)
        ]


errorView : Model -> Html Msg
errorView model =
    case model.httpError of
        Just error ->
            Utils.HtmlUtils.errorView (httpErrorToString error)

        Nothing ->
            text ""
