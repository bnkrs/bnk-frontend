module Components.Settings exposing (..)
import Html exposing (..)
-- import Html.Events exposing (onInput, onFocus, onClick)
import Html.Attributes exposing (..)
import Json.Decode as JD exposing ((:=))
-- import Json.Encode as JE
import HttpBuilder exposing (..)
import Maybe
import Task
import Navigation

-- import Utils.HttpUtils exposing (httpErrorToString)
import Globals
import Config


-- MODEL

type alias Model =
  { transactionLogging : TransactionLogging
  , recoveryMethod : RecoveryMethod
  , email : String
  , httpError: Maybe (Error String)
  }

type RecoveryMethod = EMail | Phrase | UnknownRecoveryMethod

recoveryMethodFromString : String -> RecoveryMethod
recoveryMethodFromString str =
  if str == "email"
    then EMail
    else Phrase

type TransactionLogging = Enabled | Disabled | UnknownTransactionLoggingsStatus

transactionLoggingFromBool : Bool -> TransactionLogging
transactionLoggingFromBool bo =
  if bo
    then Enabled
    else Disabled

type alias GetSettingsResponse =
  {
    transactionLogging : Bool
  , recoveryMethod : String
  , email : String
  }

initialModel : Model
initialModel =
  { transactionLogging = UnknownTransactionLoggingsStatus
  , recoveryMethod = UnknownRecoveryMethod
  , email = ""
  , httpError = Nothing
  }


-- UPDATE

urlChange : Globals.Model -> Cmd Msg
urlChange model =
  if model.apiToken == ""
    then Navigation.newUrl "#login"
    else getSettings model


type Msg = GetSettingsFailed (Error String)
  | GetSettingSuccessful (Response GetSettingsResponse)


type alias UpdateResult =
  { model : Model
  , globals : Globals.Model
  , cmd: Cmd Msg
  }


update : Msg -> Model -> Globals.Model -> UpdateResult
update msg model global =
  case msg of
    GetSettingsFailed error ->
      { model = { model | httpError = Just error }
      , globals = global
      , cmd = Cmd.none }
    GetSettingSuccessful result ->
      let
        logging = transactionLoggingFromBool result.data.transactionLogging
        recovery = recoveryMethodFromString result.data.recoveryMethod
        email = result.data.email
      in
        { model =
          { model
            | transactionLogging = logging
            , recoveryMethod = recovery
            , email = email
          }
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
doGetSettings global=
  let
    baseUrl = Config.rootUrl ++ "/user/settings"
    urlWithParams = HttpBuilder.url baseUrl [("token", global.apiToken)]
    successReader =
      jsonReader
      <| JD.object3 GetSettingsResponse
        ("transactionLogging" := JD.bool )
        ("recoveryMethod" := JD.string )
        (JD.oneOf ["email" :=  JD.string, JD.succeed ""] )
    failReader = jsonReader ( JD.at ["error"] ("message" := JD.string ))
  in
    HttpBuilder.get urlWithParams
      |> send successReader failReader



-- VIEW

view : Model -> Html Msg
view model =
  div [ class "row" ][
    div [ class "col-xs-3" ] [],
    div [ class "col-xs-6" ]
      [ text "Register view"
      ],
    div [ class "col-xs-3" ] [ ]
  ]
