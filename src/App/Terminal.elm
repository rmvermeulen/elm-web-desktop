module App.Terminal exposing (..)

import Delay
import Element exposing (..)


type alias Model =
    { commands : List String
    , lines : List String
    }


init : ( Model, Cmd Msg )
init =
    let
        helloMessage =
            PrintLine "elm $ "
    in
    ( Model [] [], Delay.after 350 Delay.Millisecond helloMessage )


type Msg
    = ExecCommand String
    | PrintLine String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExecCommand command ->
            ( { model | commands = command :: model.commands }, Cmd.none )

        PrintLine line ->
            ( { model | lines = line :: model.lines }, Cmd.none )


view : Model -> Element Msg
view model =
    text "[App.Terminal]"
