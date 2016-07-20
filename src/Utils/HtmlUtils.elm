module Utils.HtmlUtils exposing (..)

import Html exposing (..)


toActive : Bool -> String
toActive b =
    if b then
        "active"
    else
        ""


showIfTrue : Bool -> Html a -> Html a
showIfTrue b element =
    if b then
        element
    else
        text ""
