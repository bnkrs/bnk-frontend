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
            if response.data == "UsernamePasswordWrong" then
                "Wrong username or password!"
            else
                toString response.data
