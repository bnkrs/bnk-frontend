module Components.Home exposing (..)

import Globals
import Navigation
import HttpBuilder exposing (..)
import Task
import Json.Decode as JD exposing ((:=))
import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation
import Utils.HttpUtils exposing (httpErrorToString, isTokenExpired)


-- MODEL


type alias Model =
    { balance : Maybe Int
    , httpError : Maybe (Error String)
    }


initialModel : Model
initialModel =
    { balance = Nothing
    , httpError = Nothing
    }



-- UPDATE


type Msg
    = RequestBalance
    | BalanceRequestFailed (Error String)
    | BalanceRequestSuccessfull (Response Int)


update : Msg -> Model -> Globals.Model -> ( Model, Cmd Msg )
update msg model global =
    case msg of
        RequestBalance ->
            model ! [ requestBalance global ]

        BalanceRequestFailed error ->
            { model | httpError = Just error }
                ! [ if isTokenExpired error then
                        Navigation.newUrl "#login"
                    else
                        Cmd.none
                  ]

        BalanceRequestSuccessfull result ->
            { model | balance = Just result.data } ! []


urlChange : Globals.Model -> Cmd Msg
urlChange model =
    if model.apiToken == "" then
        Navigation.newUrl "#login"
    else
        requestBalance model


requestBalance : Globals.Model -> Cmd Msg
requestBalance global =
    Task.perform
        BalanceRequestFailed
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
                [ text "Current balance: ", text <| formatBalance model.balance ]
            ]
        , div [ class "col-xs-1" ] []
        ]


formatBalance : Maybe Int -> String
formatBalance balance =
    case balance of
        Just value ->
            (toString <| (toFloat value) / 10) ++ " " ++ "€"

        Nothing ->
            "No data yet"


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
