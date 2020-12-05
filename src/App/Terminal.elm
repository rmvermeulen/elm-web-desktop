module App.Terminal exposing (..)

import Element exposing (..)


type alias Model =
    { commands : List String
    }


init : Model
init =
    Model []


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    text "[App.Terminal]"
