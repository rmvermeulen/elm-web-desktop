module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Maybe
import Set exposing (Set)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ padding 10, spacing 20, Background.color (rgb 0.2 0.2 0.2) ]
        [ el [ padding 18, Background.color (rgb 0 0 0), Font.color (rgb 1 1 1) ] <| text "Elm with mdgriffith/elm-ui!"
        , el [ Font.color (rgb 0.7 0.7 1) ] <| text "this"
        , el [ Font.color (rgb 1 1 0.7) ] <| text "is a"
        , el [ Font.color (rgb 1 0.7 1) ] <| text "column"
        , row [ spacing 20, padding 10, Background.color (rgb 0 0 0) ]
            [ el [ Font.color (rgb 1 1 0.7) ] <| text "and this"
            , el [ Font.color (rgb 1 0.7 1) ] <| text "is a row"
            ]
        ]



---- PROGRAM ----
-- state serialized as json


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { view = \model -> layout [] (view model)
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
