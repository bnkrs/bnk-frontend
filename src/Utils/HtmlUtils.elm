module Utils.HtmlUtils exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onFocus, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as JD
import Regex
import String


activeIfTrue : Bool -> String
activeIfTrue b =
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


errorView : String -> Html a
errorView error =
    div [ class "alert alert-danger", attribute "role" "alert" ]
        [ span [ attribute "aria-hidden" "true", class "glyphicon glyphicon-exclamation-sign" ]
            []
        , span [ class "sr-only" ]
            [ text "Error:" ]
        , text (" " ++ error)
        ]


emailValid : String -> Bool
emailValid mail =
    let
        emailRegex =
            Regex.caseInsensitive (Regex.regex "^\\S+@\\S+\\.\\S+$")
    in
        (Regex.contains emailRegex mail) || String.length mail == 0


type alias InputFieldOptions a =
    { inputHandler : String -> a
    , focusHandler : Maybe a
    , enterHandler : Maybe ( a, a )
    , inputType : String
    , labelText : String
    , placeholderText : String
    , hasError : Bool
    , inputValue : Maybe String
    }


primaryButton : a -> Bool -> String -> Html a
primaryButton clickHandler isEnabled label =
    button
        [ class "btn btn-primary"
        , type' "submit"
        , onClick clickHandler
        , disabled <| not isEnabled
        ]
        [ text label ]


passwordStrenghBar : Int -> Html a
passwordStrenghBar strength =
    let
        progressBarColorClassName =
            case strength of
                0 ->
                    "progress-bar-danger"

                1 ->
                    "progress-bar-danger"

                2 ->
                    "progress-bar-warning"

                _ ->
                    "progress-bar-success"
    in
        div [ class "progress" ]
            [ div
                [ class <| "progress-bar " ++ progressBarColorClassName
                , attribute "role" "progressbar"
                , style [ ( "width", (toString (25 * strength)) ++ "%" ) ]
                ]
                []
            ]


textFieldWithLabel : (String -> a) -> String -> String -> Html a
textFieldWithLabel inputHandler labelText placeholderText =
    inputFieldWithLabel <| InputFieldOptions inputHandler Nothing Nothing "text" labelText placeholderText False Nothing


passwordFieldWithLabel : (String -> a) -> Maybe a -> String -> Bool -> Html a
passwordFieldWithLabel inputHandler focusHandler text hasError =
    inputFieldWithLabel <| InputFieldOptions inputHandler focusHandler Nothing "password" text text hasError Nothing


emailFieldWithLabel : (String -> a) -> String -> Bool -> Maybe String -> Html a
emailFieldWithLabel inputHandler labelText hasError value =
    inputFieldWithLabel
        { inputHandler = inputHandler
        , focusHandler = Nothing
        , enterHandler = Nothing
        , inputType = "email"
        , labelText = labelText
        , placeholderText = "user@domain.tld"
        , hasError = hasError
        , inputValue = value
        }


inputFieldWithLabel : InputFieldOptions a -> Html a
inputFieldWithLabel { inputHandler, focusHandler, enterHandler, inputType, labelText, placeholderText, hasError, inputValue } =
    let
        errorClass =
            if hasError then
                "has-error"
            else
                ""

        focus =
            case focusHandler of
                Nothing ->
                    []

                Just handler ->
                    [ onFocus handler ]

        enter =
            case enterHandler of
                Nothing ->
                    []

                Just ( msg, nopMsg ) ->
                    [ onEnter msg nopMsg ]

        valueAttribute =
            case inputValue of
                Nothing ->
                    []

                Just val ->
                    [ value val ]

        other =
            [ class "form-control"
            , placeholder placeholderText
            , type' inputType
            , onInput inputHandler
            ]

        attributes =
            List.concat [ focus, enter, valueAttribute, other ]
    in
        div [ class <| "form-group " ++ errorClass ]
            [ label []
                [ text labelText ]
            , input
                attributes
                []
            ]


onEnter : a -> a -> Attribute a
onEnter msg nopMsg =
    let
        tagger code =
            if code == 13 then
                msg
            else
                nopMsg
    in
        on "keydown" (JD.map tagger keyCode)
