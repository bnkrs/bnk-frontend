module Components.Register exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import String


-- MODEL

type alias Model =
  { userName : String
  , password : String
  , passwordConfirm : String
  }

initialModel : Model
initialModel =
  { userName = ""
  , password = ""
  , passwordConfirm = ""
  }

-- UPDATE

type Msg = NoOp

update : Msg -> Model -> Model
update msg model = model

-- VIEW
view : Int -> Html a
view model =
  div
    [ class "h1" ]
    [ text  "test" ]
