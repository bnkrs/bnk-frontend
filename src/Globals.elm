module Globals exposing (..)


type alias Model =
    { apiToken : String
    , username : String
    , endpoint : String
    }


logout : Model -> Model
logout model =
    { model | apiToken = "", username = "" }
