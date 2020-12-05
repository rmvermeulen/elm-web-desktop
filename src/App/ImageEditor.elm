module App.ImageEditor exposing (..)

import Element exposing (..)
import UndoList exposing (UndoList)


type Operation
    = AddShape
    | RemoveShape


type alias Model =
    { operations : List Operation
    , undo : UndoList (List Operation)
    }


init : ( Model, Cmd Msg )
init =
    ( Model [] (UndoList.fresh []), Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


view : Model -> Element Msg
view model =
    text "[App.ImageEditor]"
