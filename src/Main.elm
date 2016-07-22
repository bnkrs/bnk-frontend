module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.App as Html
import Navigation
import UrlParser
import String
import Result
import Globals
import Components.Register
import Components.Login
import Components.Home
import Components.Settings
import Utils.PasswordChecker


-- APP


main : Program Globals.Model
main =
    Navigation.programWithFlags
        locationParser
        { init = init
        , view = view
        , update = update
        , urlUpdate = urlUpdate
        , subscriptions = subscriptions
        }



-- MODEL


type Page
    = HomePage
    | RegisterPage
    | LoginPage
    | SettingsPage


type alias Model =
    { register : Components.Register.Model
    , login : Components.Login.Model
    , home : Components.Home.Model
    , settings : Components.Settings.Model
    , globals : Globals.Model
    , currentPage : Page
    }


init : Globals.Model -> Page -> ( Model, Cmd Msg )
init globals page =
    let
        model =
            { register = Components.Register.initialModel
            , login = Components.Login.initialModel
            , home = Components.Home.initialModel
            , settings = Components.Settings.initialModel
            , globals = globals
            , currentPage = page
            }

        command =
            commandForPage model page
    in
        ( model, command )


commandForPage : Model -> Page -> Cmd Msg
commandForPage model page =
    case page of
        HomePage ->
            Cmd.map Home <| Components.Home.urlChange model.globals

        SettingsPage ->
            Cmd.map Settings <| Components.Settings.urlChange model.globals

        _ ->
            Cmd.none



-- UPDATE


type Msg
    = Register Components.Register.Msg
    | Login Components.Login.Msg
    | Home Components.Home.Msg
    | Settings Components.Settings.Msg
    | Logout


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Register rmsg ->
            let
                newRegisterModel =
                    Components.Register.update rmsg model.register model.globals
            in
                { model | register = fst newRegisterModel } ! [ Cmd.map Register (snd newRegisterModel) ]

        Login lmsg ->
            let
                updateResult =
                    Components.Login.update lmsg model.login model.globals

                newModel =
                    { model
                        | login = updateResult.model
                        , globals = updateResult.globals
                    }
            in
                newModel ! [ Cmd.map Login updateResult.cmd ]

        Home hmsg ->
            let
                updateResult =
                    Components.Home.update hmsg model.home model.globals
            in
                { model | home = updateResult.model, globals = updateResult.globals } ! [ Cmd.map Home updateResult.cmd ]

        Settings smdg ->
            let
                updateResult =
                    Components.Settings.update smdg model.settings model.globals

                newModel =
                    { model
                        | settings = updateResult.model
                        , globals = updateResult.globals
                    }
            in
                newModel ! [ Cmd.map Settings updateResult.cmd ]

        Logout ->
            { model | globals = Globals.logout model.globals }
                ! [ Components.Login.saveToLocalstorage <| Globals.logout model.globals ]


urlUpdate : Page -> Model -> ( Model, Cmd Msg )
urlUpdate page model =
    let
        { settings, home } =
            model

        newSettings =
            { settings | phrase = [], httpError = Nothing }

        newRegister =
            Components.Register.initialModel

        newHome =
            { home | httpError = Nothing }

        newLogin =
            Components.Login.initialModel
    in
        ( { model | currentPage = page, settings = newSettings, home = newHome, register = newRegister }
        , commandForPage model page
        )


locationParser : Navigation.Parser Page
locationParser =
    Navigation.makeParser locationToPage


matchers : UrlParser.Parser (Page -> a) a
matchers =
    UrlParser.oneOf
        [ UrlParser.format HomePage (UrlParser.s "")
        , UrlParser.format HomePage (UrlParser.s "home")
        , UrlParser.format RegisterPage (UrlParser.s "register")
        , UrlParser.format LoginPage (UrlParser.s "login")
        , UrlParser.format SettingsPage (UrlParser.s "settings")
        ]


locationToPage : Navigation.Location -> Page
locationToPage location =
    location.hash
        |> String.dropLeft 1
        |> UrlParser.parse identity matchers
        |> Result.withDefault HomePage



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map Register (Utils.PasswordChecker.passwordScore Components.Register.UpdatePasswordScore)
        , Sub.map Settings (Utils.PasswordChecker.passwordScore Components.Settings.UpdatePasswordScore)
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ navBar model
        , div [ class "container" ]
            [ div [ class "content" ]
                [ case model.currentPage of
                    HomePage ->
                        model.home
                            |> Components.Home.view
                            |> Html.map Home

                    RegisterPage ->
                        model.register
                            |> Components.Register.view
                            |> Html.map Register

                    LoginPage ->
                        model.login
                            |> Components.Login.view
                            |> Html.map Login

                    SettingsPage ->
                        model.settings
                            |> Components.Settings.view
                            |> Html.map Settings
                , hr [] []
                , text (toString model)
                ]
            ]
        ]


navBar : Model -> Html Msg
navBar model =
    let
        usernameText =
            if String.length model.globals.username > 0 then
                "Hello, " ++ model.globals.username
            else
                ""

        logoutElement =
            if String.length model.globals.apiToken > 0 then
                li [] [ a [ href "", onClick Logout ] [ text "Logout" ] ]
            else
                text ""

        settingsElement =
            if String.length model.globals.apiToken > 0 then
                li
                    [ class <|
                        if model.currentPage == SettingsPage then
                            "active"
                        else
                            ""
                    ]
                    [ a [ href "#settings" ] [ text "Settings" ] ]
            else
                text ""
    in
        nav [ class "navbar navbar-default navbar-fixed-top" ]
            [ div [ class "container" ]
                [ div [ class "navbar-header" ]
                    [ a
                        [ class "navbar-brand", href "#" ]
                        [ text "bnk" ]
                    ]
                , ul [ class "nav navbar-nav" ]
                    [ li
                        [ class <|
                            if model.currentPage == HomePage then
                                "active"
                            else
                                ""
                        ]
                        [ a [ href "#" ]
                            [ text "Home" ]
                        ]
                    , settingsElement
                    ]
                , ul [ class "nav navbar-nav navbar-right " ]
                    [ logoutElement
                    ]
                ]
            ]
