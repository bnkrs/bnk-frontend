port module Components.Register exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onFocus, onClick)
import String
import Regex
import Json.Decode as JD exposing ((:=))
import Json.Encode as JE
import Task
import Maybe
import Navigation
import HttpBuilder exposing (..)
import Globals
import Utils.HttpUtils exposing (httpErrorToString)
import Utils.HtmlUtils exposing (..)


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
    , httpError : Maybe (Error String)
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
    , httpError = Maybe.Nothing
    }



-- PORTS


port checkPassword : String -> Cmd msg


port passwordScore : (Int -> msg) -> Sub msg



-- UPDATE


type Msg
    = ChangeUsername String
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


update : Msg -> Model -> Globals.Model -> ( Model, Cmd Msg )
update msg model globals =
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
            let
                command =
                    if not <| emailEmpty model then
                        registerUserWithEmail model globals
                    else
                        registerUserWithPhrase model globals
            in
                model ! [ command ]

        RegistrationCompleted response ->
            { model
                | wasRegistrationSuccessfull = response.data
                , password = ""
                , passwordConfirm = ""
            }
                ! [ Navigation.newUrl "#login" ]

        RegistrationCompletedWithPhase response ->
            let
                wasSuccessfull =
                    fst response.data

                phrase =
                    snd response.data
            in
                { model
                    | wasRegistrationSuccessfull = wasSuccessfull
                    , phrase = phrase
                    , password = ""
                    , passwordConfirm = ""
                }
                    ! []

        RegistriationFailed error ->
            { model
                | httpError = Just error
                , password = ""
                , passwordConfirm = ""
            }
                ! []


registerUserWithEmail : Model -> Globals.Model -> Cmd Msg
registerUserWithEmail model globals =
    Task.perform
        RegistriationFailed
        RegistrationCompleted
        (doMailRegister model globals)


registerUserWithPhrase : Model -> Globals.Model -> Cmd Msg
registerUserWithPhrase model globals =
    Task.perform
        RegistriationFailed
        RegistrationCompletedWithPhase
        (doPhraseRegister model globals)


doMailRegister : Model -> Globals.Model -> Task.Task (Error String) (Response Bool)
doMailRegister model globals =
    let
        registerUrl =
            globals.endpoint ++ "/user/new"

        registerSuccessReader =
            jsonReader ("success" := JD.bool)

        registerFailReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))

        body =
            modelToJson model
    in
        HttpBuilder.post registerUrl
            |> withJsonBody body
            |> withHeader "Content-Type" "application/json"
            |> send registerSuccessReader registerFailReader


doPhraseRegister : Model -> Globals.Model -> Task.Task (Error String) (Response ( Bool, List String ))
doPhraseRegister model globals =
    let
        registerUrl =
            globals.endpoint ++ "/user/new"

        registerFailReader =
            jsonReader (JD.at [ "error" ] ("message" := JD.string))

        registerSuccessReader =
            jsonReader <| JD.object2 (,) ("success" := JD.bool) ("phrase" := JD.list JD.string)

        body =
            modelToJson model
    in
        HttpBuilder.post registerUrl
            |> withJsonBody body
            |> withHeader "Content-Type" "application/json"
            |> send registerSuccessReader registerFailReader


modelToJson : Model -> JE.Value
modelToJson model =
    let
        usernamePassword =
            [ ( "username", JE.string model.username )
            , ( "password", JE.string model.password )
            ]

        recovery =
            if not <| emailEmpty model then
                [ ( "recoveryMethod", JE.string "email" ), ( "email", JE.string model.email ) ]
            else
                [ ( "recoveryMethod", JE.string "phrase" ) ]
    in
        JE.object <| List.append usernamePassword recovery



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "col-sm-3" ] []
        , div [ class "col-sm-6" ] [ errorView model, phraseView model, formView model ]
        , div [ class "col-sm-3" ] []
        ]


errorView : Model -> Html Msg
errorView model =
    case model.httpError of
        Just error ->
            Utils.HtmlUtils.errorView (httpErrorToString error)

        Nothing ->
            text ""


phraseView : Model -> Html Msg
phraseView model =
    let
        hasPhrase =
            List.length model.phrase > 0

        phraseStr =
            List.foldl (\word phrase -> phrase ++ " " ++ word) " " model.phrase

        expplaination =
            """You have registered without an email address.
         If you forget your password, you can revocer your account using this recovery phrase:

      """
    in
        if hasPhrase then
            div [ class "alert alert-success lead", attribute "role" "alert" ]
                [ text expplaination
                , br [] []
                , b [] [ text phraseStr ]
                ]
        else
            text ""


formView : Model -> Html Msg
formView model =
    if model.wasRegistrationSuccessfull then
        a [ href "#login", class "text-center lead" ] [ text "Click here to log in!" ]
    else
        div [ class "panel panel-primary register-form" ]
            [ div [ class "panel-heading" ] [ text "Register" ]
            , div [ class "panel-body" ]
                [ textFieldWithLabel ChangeUsername "Username" "Username"
                , passwordFieldWithLabel
                    ChangePassword
                    (Just FocusPassword)
                    "Password"
                    ((model.passwordWasFocussed) && not (passwordsAreOk model))
                , label [] [ text "Password Strength" ]
                , div [ class "progress" ]
                    [ div
                        [ class <| "progress-bar " ++ progressBarColorClassName model
                        , attribute "role" "progressbar"
                        , style [ ( "width", (toString (calculateWith model)) ++ "%" ) ]
                        ]
                        []
                    ]
                , passwordFieldWithLabel
                    ChangePassswordConfirm
                    (Just FocusPasswordConfirm)
                    "Confirm Password"
                    ((model.passwordConfirmWasFocussed) && not (passwordsAreOk model))
                , emailFieldWithLabel
                    ChangeEmail
                    "E-Mail (optional)"
                    (not (emailValid model))
                    Nothing
                , button
                    [ class "btn btn-primary"
                    , type' "submit"
                    , disabled <| not (isValid model)
                    , onClick Register
                    ]
                    [ text "Register" ]
                ]
            ]


progressBarColorClassName : Model -> String
progressBarColorClassName model =
    case model.passwordScore of
        0 ->
            "progress-bar-danger"

        1 ->
            "progress-bar-danger"

        2 ->
            "progress-bar-warning"

        _ ->
            "progress-bar-success"


passwordsAreOk : Model -> Bool
passwordsAreOk model =
    model.password == model.passwordConfirm && model.passwordScore > 1


usernameOk : Model -> Bool
usernameOk model =
    (String.length model.username) > 0


emailValid : Model -> Bool
emailValid model =
    let
        emailRegex =
            Regex.caseInsensitive (Regex.regex "^\\S+@\\S+\\.\\S+$")
    in
        (Regex.contains emailRegex model.email) || String.length model.email == 0


emailEmpty : Model -> Bool
emailEmpty model =
    (String.length model.email) == 0


isValid : Model -> Bool
isValid model =
    (passwordsAreOk model) && (usernameOk model) && (emailValid model)


calculateWith : Model -> Int
calculateWith model =
    25 * model.passwordScore
