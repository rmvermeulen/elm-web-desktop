module Desktop exposing (..)

import Desktop.Icon as Icon


type alias Program =
    { name : String }


type alias Process =
    { id : Int
    , program : Program
    }


type alias Model =
    { nextId : Int
    , programs : List Program
    , processes : List Process
    , icons : List Icon.Model
    }


type Msg
    = StopProcess Int
    | StartProcess Program


init : Model
init =
    let
        programs =
            [ Program "TextEditor"
            , Program "ImageEditor"
            , Program "Some Game"
            , Program "IntegratedTerminal"
            , Program "Photos"
            , Program "Some stupidly long name"
            ]
    in
    { programs = programs
    , nextId = 0
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
        |> update (StartProcess <| Program "TextEditor")


update : Msg -> Model -> Model
update msg model =
    case msg of
        StopProcess id ->
            { model
                | processes =
                    model.processes
                        |> List.filter (\p -> p.id /= id)
            }

        StartProcess program ->
            let
                process =
                    Process model.nextId program
            in
            { model
                | nextId = model.nextId + 1
                , processes = process :: model.processes
            }
