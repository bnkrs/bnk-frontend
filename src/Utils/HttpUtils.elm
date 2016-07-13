module Utils.HttpUtils exposing (..)

import HttpBuilder exposing (..)


httpErrorToString : HttpBuilder.Error String -> String
httpErrorToString error =
    case error of
        UnexpectedPayload str ->
            """ Internal Error!
          You can't do anything about this, contact someone who does tech stuff!
      """

        NetworkError ->
            """ Network Error! Make sure you have an internet connection and try again.
          If that does not work, contact someone who administrates your BNK installation!
      """

        Timeout ->
            """ Network Error! Make sure you have an internet connection and try again.
          If that does not work, contact someone who administrates your BNK installation!
      """

        BadResponse response ->
            case response.data of
                "UsernamePasswordWrong" ->
                    "Wrong username or password!"

                "UserExists" ->
                    "An account with this username already exists"

                _ ->
                    toString response.data


isTokenExpired : HttpBuilder.Error String -> Bool
isTokenExpired error =
    case error of
        BadResponse response ->
            if response.data == "NotAuthenticated" then
                True
            else
                False

        _ ->
            False
