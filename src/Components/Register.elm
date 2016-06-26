port module Components.Register exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onFocus, onClick)
import String
import Regex
import Http
import Json.Decode as JD exposing((:=))
import Json.Encode as JE
import Task
import Maybe

import Config
import Utils.HttpUtils as HttpUtils


-- MODEL

type alias Model =
  { username : String
  , password : String
  , passwordConfirm : String
  , email : String
  , passwordWasFocussed : Bool
  , phraseWasFocussed : Bool
  , passwordConfirmWasFocussed : Bool
  , passwordScore : Int
  , phrase : List String
  , wasRegistrationSuccessfull : Bool
  , conncectionError : Maybe Http.Error
  }


initialModel : Model
initialModel =
  { username = ""
  , password = ""
  , passwordConfirm = ""
  , email = ""
  , passwordWasFocussed = False
  , phraseWasFocussed = False
  , passwordConfirmWasFocussed = False
  , passwordScore = 0
  , phrase = []
  , wasRegistrationSuccessfull = False
  , conncectionError = Maybe.Nothing
  }


-- PORTS

port checkPassword : String -> Cmd msg


port passwordScore : (Int -> msg) -> Sub msg


-- UPDATE

type Msg = ChangeUsername String
  | ChangePassword String
  | ChangePassswordConfirm String
  | ChangeEmail String
  | FocusPassword
  | FocusPasswordConfirm
  | UpdatePasswordScore Int
  | Register
  | RegistrationCompleted Bool
  | RegistrationCompletedWithPhase ( Bool, List String )
  | RegistriationFailed Http.Error


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeUsername name ->
      ( { model | username = name }, Cmd.none )
    ChangePassword password ->
      ( { model | password = password }, checkPassword password )
    ChangePassswordConfirm passwordConfirm ->
      ( { model | passwordConfirm = passwordConfirm }, Cmd.none )
    ChangeEmail mail ->
      ( { model | email = mail }, Cmd.none )
    FocusPassword ->
      ( { model | passwordWasFocussed = True}, Cmd.none)
    FocusPasswordConfirm ->
      ( { model | passwordConfirmWasFocussed = True}, Cmd.none)
    UpdatePasswordScore score ->
      ( { model | passwordScore = score }, Cmd.none )
    Register ->
      ( model, registerUser model )
    RegistrationCompleted success ->
      ( {model | wasRegistrationSuccessfull = success }, Cmd.none )
    RegistrationCompletedWithPhase (success, phrase) ->
      ( {model | wasRegistrationSuccessfull = success, phrase = phrase }, Cmd.none)
    RegistriationFailed error ->
      ( { model | conncectionError = Maybe.Just error }, Cmd.none )


registerUser : Model -> Cmd Msg
registerUser model =
  let
    registerUrl = Config.rootUrl ++ "/user/new"
    successDecoder = "success" := JD.bool
    phraseDecoder = JD.object2 (,) ("success" := JD.bool) ("phrase" := JD.list JD.string)
    body = Http.string <| modelToJson model
  in
    if emailValid model then
      Task.perform
        RegistriationFailed
        RegistrationCompleted
        (HttpUtils.post successDecoder registerUrl body)
    else
      Task.perform
        RegistriationFailed
        RegistrationCompletedWithPhase
        (HttpUtils.post phraseDecoder registerUrl body)


modelToJson : Model -> String
modelToJson model =
  let
    usernamePassword =
      [ ("username", JE.string model.username)
      , ("password", JE.string model.password) ]
    recovery = if emailValid model
      then [ ("recoveryMethod", JE.string "email"), ("email", JE.string model.email) ]
      else [ ("recoveryMethod", JE.string "phrase") ]
  in
    JE.encode 0 <| JE.object (List.append usernamePassword recovery)




-- VIEW

view : Model -> Html Msg
view model =
  div [ class "row" ][
    div [ class "col-xs-4" ] [],
    div [ class "col-xs-4" ][ formView model ],
    div [ class "col-xs-4" ] []
  ]


formView : Model -> Html Msg
formView model =
  div [class "panel panel-primary register-form"]
    [
      div [class "panel-heading"] [text "Register"],
      div [class "panel-body"]
        [ div [ class <| "form-group"]
            [ label [ for "userName" ]
                [ text "Username" ]
            , input [ class "form-control", id "userName"
              , placeholder "Username", type' "text", onInput ChangeUsername ]
                []
            ]
        , div [ class <| "form-group " ++ passwordValidationClassName model]
            [ label [ for "password" ]
                [ text "Password" ]
            , input [ class "form-control", id "password"
              , placeholder "Password", type' "password"
              , onInput ChangePassword
              , onFocus FocusPassword ]
                []
            ]
        , label [] [text "Password Strength"]
        , div [ class "progress"]
          [
            div [ class <| "progress-bar " ++ progressBarColorClassName model
                , attribute "role" "progressbar"
                , style [ ("width", (toString (calculateWith model)) ++ "%" )]]
                []
          ]
        , div [ class <| "form-group " ++ passwordConfirmValidationClassName model ]
            [ label [ for "passwordConfirm" ]
                [ text "Confirm Password" ]
            , input [ class "form-control", id "passwordConfirm"
              , placeholder "Confirm Password", type' "password"
              , onInput ChangePassswordConfirm
              , onFocus FocusPasswordConfirm ]
                []
            ]
        , div [ class <| "form-group " ++ emailValidationClassName model ]
            [ label [ for "email" ]
                [ text "E-Mail (optional)" ]
            , input [ class "form-control", id "email"
              , placeholder "user@domain.tld", type' "email"
              , onInput ChangeEmail]
                []
            ]
        , button [ class "btn btn-primary", type' "submit"
          , disabled <| not (isValid model)
          , onClick Register]
            [ text "Submit" ]
        ]
    ]


passwordValidationClassName : Model -> String
passwordValidationClassName model =
  if passwordsAreOk model || passwordNotFocussed model then "" else "has-error"


passwordConfirmValidationClassName : Model -> String
passwordConfirmValidationClassName model =
  if passwordsAreOk model || passwordConfirmNotFocussed model then "" else "has-error"


emailValidationClassName : Model -> String
emailValidationClassName model =
  if emailValid model || emailEmpty model then "" else "has-error"


progressBarColorClassName : Model -> String
progressBarColorClassName model =
  case model.passwordScore of
    0 -> "progress-bar-danger"
    1 -> "progress-bar-danger"
    2 -> "progress-bar-warning"
    _ -> "progress-bar-success"


passwordsAreTheSame : Model -> Bool
passwordsAreTheSame model = model.password == model.passwordConfirm


passwordsAreOk : Model -> Bool
passwordsAreOk model =
  passwordsAreTheSame model && model.passwordScore > 1


usernameOk : Model -> Bool
usernameOk model = (String.length model.username) > 0


passwordConfirmNotFocussed : Model -> Bool
passwordConfirmNotFocussed model = not model.passwordConfirmWasFocussed

passwordNotFocussed : Model -> Bool
passwordNotFocussed model = not model.passwordWasFocussed


emailValid : Model -> Bool
emailValid model =
  let
    emailRegex = Regex.caseInsensitive (Regex.regex "^\\S+@\\S+\\.\\S+$")
  in
    (Regex.contains emailRegex model.email) || String.length model.email == 0


emailEmpty : Model -> Bool
emailEmpty model = (String.length model.email) == 0


isValid : Model -> Bool
isValid model = (passwordsAreOk model) && (usernameOk model)  && (emailValid model)


calculateWith : Model -> Int
calculateWith model =
  25 * model.passwordScore
