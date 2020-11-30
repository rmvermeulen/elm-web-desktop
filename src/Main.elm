module Main exposing (..)

import Browser
import Desktop
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input


type alias Model =
    Desktop.Model


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( Desktop.init, Cmd.none )


type Msg
    = DesktopMsg Desktop.Msg


update : Msg -> Model -> Model
update msg model =
    case msg of
        DesktopMsg desktopMsg ->
            Desktop.update desktopMsg model


view : Model -> Element Msg
view model =
    Desktop.view model
        |> map DesktopMsg



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = \model -> layout [] (view model)
        , init = init
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always Sub.none
        }
