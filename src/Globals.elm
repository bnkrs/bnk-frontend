module Globals exposing (..)

type alias Model =
  { apiToken : String
  }

initialModel : Model
initialModel = { apiToken = "" }
