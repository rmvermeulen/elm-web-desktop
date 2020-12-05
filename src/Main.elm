module Main exposing (..)

import Browser
import Desktop
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)


type alias Model =
    Desktop.Model


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init _ =
    Desktop.init
        |> Tuple.mapSecond (Cmd.map DesktopMsg)


type Msg
    = DesktopMsg Desktop.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DesktopMsg desktopMsg ->
            Desktop.update desktopMsg model
                |> Tuple.mapSecond (Cmd.map DesktopMsg)


view : Model -> Html Msg
view model =
    layout []
        (Desktop.view model
            |> map DesktopMsg
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Desktop.subscriptions model
        |> Sub.map DesktopMsg



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
        }
