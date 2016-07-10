import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Navigation
import UrlParser
import String
import Result

import Globals

import Components.Register
import Components.Login

-- APP

main : Program Never
main =
  Navigation.program
    locationParser
    { init = init
    , view = view
    , update = update
    , urlUpdate = urlUpdate
    , subscriptions = subscriptions
    }


-- MODEL

type Page = HomePage
  | RegisterPage
  | LoginPage


type alias Model =
  { register : Components.Register.Model
  , login : Components.Login.Model
  , currentPage : Page
  , globals : Globals.Model
  }


init : Page -> ( Model, Cmd Msg )
init page =
  let
    model =
      { register = Components.Register.initialModel
      , login = Components.Login.initialModel
      , globals = Globals.initialModel
      , currentPage = page
      }
    command = switchPageIfNeeded model
  in
    ( model, command )


switchPageIfNeeded : Model -> Cmd Msg
switchPageIfNeeded model =
  if model.globals.apiToken == ""
    then Navigation.newUrl "#login" else Cmd.none

-- UPDATE

type Msg = Register Components.Register.Msg
  | Login Components.Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg)
update msg model =
  case msg of
    Register rmsg ->
      let
        newRegisterModel = Components.Register.update rmsg model.register
      in
        { model | register = fst newRegisterModel } ! [ Cmd.map Register ( snd newRegisterModel ) ]
    Login lmsg ->
      let
        updateResult = Components.Login.update lmsg model.login model.globals
        newModel =
          { model | login = updateResult.model
          , globals = updateResult.globals}
      in
        newModel ! [ Cmd.map Login updateResult.cmd ]


urlUpdate : Page -> Model -> ( Model, Cmd Msg)
urlUpdate page model = ({ model | currentPage = page}, Cmd.none)


locationParser : Navigation.Parser Page
locationParser = Navigation.makeParser locationToPage


matchers : UrlParser.Parser (Page -> a) a
matchers =
  UrlParser.oneOf
    [ UrlParser.format HomePage (UrlParser.s "")
    , UrlParser.format HomePage (UrlParser.s "home")
    , UrlParser.format RegisterPage (UrlParser.s "register")
    , UrlParser.format LoginPage (UrlParser.s "login")
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
  Sub.map Register (Components.Register.passwordScore Components.Register.UpdatePasswordScore)


-- VIEW

view : Model -> Html Msg
view model =
  div []
      [ navBar
      , div [ class "container" ]
          [ div [ class "content" ]
            [ case model.currentPage of
                  HomePage ->
                    text "Homepage"
                  RegisterPage ->
                    model.register
                      |> Components.Register.view
                      |> Html.map Register
                  LoginPage ->
                    model.login
                      |> Components.Login.view
                      |> Html.map Login
                , hr [] []
                , text (toString model)
            ]
          ]
  ]


navBar : Html a
navBar =
    nav [ class "navbar navbar-inverse navbar-fixed-top" ]
        [ div [ class "container" ]
            [ div [ class "navbar-header" ]
                [ button [ attribute "aria-controls" "navbar", attribute "aria-expanded" "false", class "navbar-toggle collapsed", attribute "data-target" "#navbar", attribute "data-toggle" "collapse", type' "button" ]
                    [ span [ class "sr-only" ]
                        [ text "Toggle navigation" ]
                    , span [ class "icon-bar" ]
                        []
                    , span [ class "icon-bar" ]
                        []
                    , span [ class "icon-bar" ]
                        []
                    ]
                , a [ class "navbar-brand", href "#" ]
                    [ text "bnk" ]
                ]
            , div [ class "collapse navbar-collapse", id "navbar" ]
                [ ul [ class "nav navbar-nav" ]
                    [ li [ class "active" ]
                        [ a [ href "#" ]
                            [ text "Home" ]
                        ]
                    , li []
                        [ a [ href "#about" ]
                            [ text "About" ]
                        ]
                    , li []
                        [ a [ href "#contact" ]
                            [ text "Contact" ]
                        ]
                    ]
                ]
            ]
        ]
