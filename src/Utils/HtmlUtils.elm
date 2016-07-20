module Utils.HtmlUtils exposing (..)

import Html exposing (..)
import Html.Events exposing (onInput, onFocus, onClick, on, keyCode)
import Html.Attributes exposing (..)
import Json.Decode as JD


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
