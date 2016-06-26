module Utils.HttpUtils exposing (post)
import Json.Decode as Json
import Http exposing (Error, Body, fromJson, send, defaultSettings)
import Task exposing (Task)

post : Json.Decoder value -> String -> Body -> Task Error value
post decoder url body =
  let request =
        { verb = "POST"
        , headers = [("Content-type", "application/json")]
        , url = url
        , body = body
        }
  in
      fromJson decoder (send defaultSettings request)
