module App.Game exposing (..)

import Element exposing (..)


type alias Model =
    { lives : Int
    , score : Int
    }


init : Model
init =
    Model 0 0


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    text "[App.Game]"
