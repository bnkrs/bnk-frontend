port module Utils.PasswordChecker exposing (..)


port checkPassword : String -> Cmd msg


port passwordScore : (Int -> msg) -> Sub msg
