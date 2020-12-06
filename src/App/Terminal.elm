module App.Terminal exposing (..)

import Delay
import Element exposing (..)
import Element.Input as Input
import FileSystem as Fs
import Helpers


type alias ProcessInfo =
    { id : Int
    , name : String
    }


type alias Model =
    { inputEnabled : Bool
    , currentLine : String
    , commands : List String
    , lines : List String
    }


type Msg
    = EditLine String
    | ExecLine
    | Print String
    | EnableInput
    | DisableInput


delayMs : Float -> Msg -> Cmd Msg
delayMs ms =
    Delay.after ms Delay.Millisecond


init : ( Model, Cmd Msg )
init =
    ( Model False "" [] []
    , Cmd.batch
        [ delayMs 50 (Print "Terminal ready")
        , delayMs 150 EnableInput
        ]
    )


update : List ProcessInfo -> Fs.DirInfo -> Msg -> Model -> ( Model, Cmd Msg )
update processes root msg model =
    case msg of
        ExecLine ->
            let
                command =
                    model.currentLine

                ( output, execCmd ) =
                    execCommand processes root command
            in
            { model
                | commands = model.commands ++ [ command ]
                , currentLine = ""
            }
                |> update processes root (Print output)
                |> (\( m, printCmd ) -> ( m, Cmd.batch [ execCmd, printCmd ] ))

        Print line ->
            ( { model | lines = model.lines ++ [ line ] }, Cmd.none )

        EditLine line ->
            ( { model | currentLine = line }, Cmd.none )

        EnableInput ->
            ( { model | inputEnabled = True }, Cmd.none )

        DisableInput ->
            ( { model | inputEnabled = False }, Cmd.none )


type Printable
    = StartOfSequence
    | PrintString String
    | EndOfSequence


printSequence : Int -> List String -> List (Cmd Msg)
printSequence interval lines =
    let
        printables =
            lines |> List.map PrintString |> (::) StartOfSequence |> List.append [ EndOfSequence ]
    in
    printables
        |> List.indexedMap
            (\n printable ->
                let
                    ms =
                        toFloat <| (n + 1) * interval
                in
                case printable of
                    StartOfSequence ->
                        delayMs 0 DisableInput

                    PrintString s ->
                        delayMs ms (Print s)

                    EndOfSequence ->
                        delayMs ms EnableInput
            )


execCommand : List ProcessInfo -> Fs.DirInfo -> String -> ( String, Cmd Msg )
execCommand processes root command =
    case command of
        "help" ->
            ( "[ Help ]"
            , [ "help - show this information"
              , "ls - list items in current folder"
              , "top - show active processes"
              , ""
              ]
                |> printSequence 25
                |> Cmd.batch
            )

        "ls" ->
            let
                lineCmds =
                    root.entries
                        |> List.filterMap
                            (\entry ->
                                case entry of
                                    Fs.File { name } ->
                                        Just name

                                    Fs.Dir { name } ->
                                        Just <| name ++ "/"
                            )
                        |> printSequence 25
            in
            ( "entries:"
            , Cmd.batch lineCmds
            )

        "top" ->
            let
                lineCmds =
                    processes
                        |> List.map (\{ id, name } -> "[" ++ String.fromInt id ++ "] " ++ name)
                        |> printSequence 25
            in
            ( "processes:", Cmd.batch lineCmds )

        _ ->
            ( "[Ran \"" ++ command ++ "\"]", delayMs 250 EnableInput )


view : Model -> Element Msg
view model =
    let
        lines =
            List.map text model.lines

        input =
            Input.text [ Helpers.onEnter ExecLine ]
                { onChange = EditLine
                , label = Input.labelLeft [] (text "$")
                , placeholder = Nothing
                , text = model.currentLine
                }

        children =
            if model.inputEnabled then
                lines ++ [ input ]

            else
                lines
    in
    column [] children
