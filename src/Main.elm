module Main exposing (main)

import Browser
import Element exposing (..)
import Html exposing (Html)


main =
    Browser.sandbox { init = init, update = update, view = view }


init : Model
init =
    "Hello, FlipFlop!"


type alias Model =
    String


update : Model -> msg -> Model
update model msg =
    model


view : Model -> Html msg
view model =
    layout [] <| text model
