module Globals exposing (..)


type alias Model =
  { apiToken : String
  , username : String
  , endpoint : String
  }


initialModel : Model
initialModel = { apiToken = "",  username = "", endpoint = "" }
