port module Components.Register exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onFocus, onClick)
import String
import Regex
import Json.Decode as JD exposing((:=))
import Json.Encode as JE
import Task
import Maybe
import Navigation
import HttpBuilder exposing (..)

import Config


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
  , conncectionError : Maybe (Error String)
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
  | RegistrationCompleted (Response Bool)
  | RegistrationCompletedWithPhase (Response ( Bool, List String ))
  | RegistriationFailed (Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ChangeUsername name ->
      { model | username = name } ! []
    ChangePassword password ->
      { model | password = password } ! [ checkPassword password ]
    ChangePassswordConfirm passwordConfirm ->
      { model | passwordConfirm = passwordConfirm } ! []
    ChangeEmail mail ->
      { model | email = mail } ! []
    FocusPassword ->
      { model | passwordWasFocussed = True } ! []
    FocusPasswordConfirm ->
      { model | passwordConfirmWasFocussed = True } ! []
    UpdatePasswordScore score ->
      { model | passwordScore = score } ! []
    Register ->
      model ! [ if emailValid model then registerUserWithEmail model else registerUserWithPhrase model ]
    RegistrationCompleted response ->
      { model | wasRegistrationSuccessfull = response.data } ! [ Navigation.newUrl "#login" ]
    RegistrationCompletedWithPhase response ->
      let
        wasSuccessfull = fst response.data
        phrase = snd response.data
      in
      { model | wasRegistrationSuccessfull = wasSuccessfull, phrase = phrase } ! []
    RegistriationFailed error ->
      { model | conncectionError = Just error } ! []



registerUserWithEmail : Model -> Cmd Msg
registerUserWithEmail model =
  Task.perform
    RegistriationFailed
    RegistrationCompleted
    (doMailRegister model)


registerUserWithPhrase : Model -> Cmd Msg
registerUserWithPhrase model =
  Task.perform
    RegistriationFailed
    RegistrationCompletedWithPhase
    (doPhraseRegister model)


doMailRegister : Model -> Task.Task (Error String) (Response Bool)
doMailRegister model  =
  let
    registerUrl = Config.rootUrl ++ "/user/new"
    registerSuccessReader = jsonReader ("success" := JD.bool)
    registerFailReader = jsonReader ( JD.at ["error"] ("message" := JD.string ))
    body = modelToJson model
  in
    HttpBuilder.post registerUrl
      |> withJsonBody body
      |> withHeader "Content-Type" "application/json"
      |> send registerSuccessReader registerFailReader


doPhraseRegister : Model -> Task.Task (Error String) (Response (Bool, List String))
doPhraseRegister model  =
  let
    registerUrl = Config.rootUrl ++ "/user/new"
    registerFailReader = jsonReader ( JD.at ["error"] ("message" := JD.string ))
    registerSuccessReader =
      jsonReader <| JD.object2 (,) ("success" := JD.bool) ("phrase" := JD.list JD.string)
    body = modelToJson model
  in
    HttpBuilder.post registerUrl
      |> withJsonBody body
      |> withHeader "Content-Type" "application/json"
      |> send registerSuccessReader registerFailReader


modelToJson : Model -> JE.Value
modelToJson model =
  let
    usernamePassword =
      [ ("username", JE.string model.username)
      , ("password", JE.string model.password) ]
    recovery = if not <| emailEmpty model
      then [ ("recoveryMethod", JE.string "email"), ("email", JE.string model.email) ]
      else [ ("recoveryMethod", JE.string "phrase") ]
  in
    JE.object <| List.append usernamePassword recovery


-- VIEW

view : Model -> Html Msg
view model =
  div [ class "row" ][
    div [ class "col-xs-3" ] [],
    div [ class "col-xs-6" ][ errorView model, phraseView model, formView model ],
    div [ class "col-xs-3" ] []
  ]


errorView : Model -> Html Msg
errorView model =
  case model.conncectionError of
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
    hasPhrase = List.length model.phrase > 0
    phraseStr = List.foldl (\word phrase -> phrase ++ " " ++ word) " " model.phrase
    expplaination =
      """You have registered without an email address.
         If you forget your password, you can revocer your account using this recovery phrase:

      """
  in
    if hasPhrase
      then
        div [ class "alert alert-success lead", attribute "role" "alert" ]
        [
          text expplaination, br [] [], b [] [text phraseStr ]
        ]
      else
        text ""

formView : Model -> Html Msg
formView model =
  if model.wasRegistrationSuccessfull
    then
      a [ href "#login", class "text-center lead"] [text "Click here to log in!"]
    else
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
                [ text "Register" ]
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
