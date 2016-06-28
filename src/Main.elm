import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as Html
import Navigation
import UrlParser
import String
import Result

import Components.Register
import Components.Login

-- APP

main : Program Never
main =
  Navigation.program
    locationParser
    { init = init, view = view, update = update, urlUpdate = urlUpdate, subscriptions = subscriptions }


-- MODEL

type Page = HomePage
  | RegisterPage
  | LoginPage


type alias Model =
  { register : Components.Register.Model
  , login : Components.Login.Model
  , currentPage : Page
  }


init : Page -> ( Model, Cmd Msg )
init page = (
      { register = Components.Register.initialModel
      , login = Components.Login.initialModel
      , currentPage = page
      },
    Cmd.none )


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
        ( { model | register = fst newRegisterModel }, Cmd.map Register ( snd newRegisterModel ) )
    Login lmsg ->
      let
        newLoginModel = Components.Login.update lmsg model.login
      in
        ( { model | login = fst newLoginModel }, Cmd.map Login ( snd newLoginModel ) )



urlUpdate : Page -> Model -> ( Model, Cmd Msg)
urlUpdate page model = (model, Cmd.none)


locationParser : Navigation.Parser Page
locationParser = Navigation.makeParser locationToPage

matchers : UrlParser.Parser (Page -> a) a
matchers =
  UrlParser.oneOf
    [ UrlParser.format HomePage (UrlParser.s "")
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
  div [ class "container-fluid" ][
    case model.currentPage of
      HomePage ->
        div [] []
      RegisterPage ->
        model.register
          |> Components.Register.view
          |> Html.map Register
      LoginPage ->
        model.login
          |> Components.Login.view
          |> Html.map Login
    , text (toString model)
  ]
