module Globals exposing (..)
import Json.Encode as JE


type alias Model =
  { apiToken : String
  , username : String
  }


initialModel : Model
initialModel = { apiToken = "",  username = "" }


tokenJson : Model -> JE.Value
tokenJson model =
  JE.object
  [ ("token", JE.string model.apiToken) ]


tokenJsonString : Model -> String
tokenJsonString model =
  JE.encode 0 <| tokenJson model
