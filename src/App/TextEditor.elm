module App.TextEditor exposing (..)

import Element exposing (..)
import UndoList exposing (UndoList)


type alias Model =
    { lines : List String
    , undo : UndoList (List String)
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
    text "[App.TextEditor]"
