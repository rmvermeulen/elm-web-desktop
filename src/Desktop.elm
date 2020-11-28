module Desktop exposing (..)

import Desktop.Icon as Icon


type alias Program =
    { name : String }


type alias Process =
    { id : Int
    , program : Program
    }


type alias Model =
    { programs : List Program
    , processes : List Process
    , icons : List Icon.Model
    }


type Msg
    = NoOp


init : Model
init =
    let
        programs =
            [ Program "TextEditor"
            , Program "ImageEditor"
            , Program "Some Game"
            , Program "Work stuff"
            , Program "Photos"
            , Program "Some stupidly long name"
            ]
    in
    { programs = programs
    , processes = []
    , icons =
        programs
            |> List.map
                (\{ name } ->
                    let
                        description =
                            "about:" ++ name
                    in
                    Icon.Model name description "logo.svg"
                )
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
