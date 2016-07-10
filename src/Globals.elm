module Globals exposing (..)

type alias Model =
  { apiToken : String
  , username : String
  }

initialModel : Model
initialModel = { apiToken = "",  username = "" }
