module App.Terminal exposing (..)

import Delay
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
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
    , cwd : List String
    }


type Msg
    = EditLine String
    | ExecLine
    | Print String
    | EnableInput
    | DisableInput
    | ChangeCwd (List String)


delayMs : Float -> Msg -> Cmd Msg
delayMs ms =
    Delay.after ms Delay.Millisecond


init : ( Model, Cmd Msg )
init =
    ( Model False "" [] [] []
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

                cmdRoot =
                    Fs.dirAtPath model.cwd root
                        |> Maybe.withDefault root

                ( output, execCmd ) =
                    execCommand processes cmdRoot command
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

        ChangeCwd cwd ->
            ( if List.isEmpty cwd then
                { model | cwd = cwd }

              else
                case Fs.dirAtPath cwd root of
                    Just _ ->
                        { model | cwd = cwd }

                    Nothing ->
                        model
            , Cmd.none
            )


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
    let
        words =
            String.split " " command
    in
    case words of
        [ "help" ] ->
            ( "[ Help ]"
            , [ "help - show this information"
              , "ls - list items in current folder"
              , "cd - change directory"
              , "top - show active processes"
              , ""
              ]
                |> printSequence 25
                |> Cmd.batch
            )

        "ls" :: target ->
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

        "cd" :: targets ->
            case targets of
                [] ->
                    ( "", delayMs 25 (ChangeCwd []) )

                [ target ] ->
                    ( "", delayMs 25 (ChangeCwd <| String.split "/" target) )

                _ ->
                    ( "takes 1 argument", Cmd.none )

        [ "top" ] ->
            let
                lineCmds =
                    processes
                        |> List.map (\{ id, name } -> "[" ++ String.fromInt id ++ "] " ++ name)
                        |> printSequence 25
            in
            ( "processes:", Cmd.batch lineCmds )

        _ ->
            ( "Unknown command \"" ++ command ++ "\"", delayMs 50 EnableInput )


view : Model -> Element Msg
view model =
    let
        lines =
            List.map text model.lines

        input =
            Input.text
                [ alignBottom
                , Helpers.onEnter ExecLine
                , Background.color (rgba 0 0 0 0)
                , Border.widthEach
                    { bottom = 2
                    , left = 0
                    , right = 0
                    , top = 0
                    }
                ]
                { onChange = EditLine
                , label =
                    Input.labelLeft []
                        (model.cwd
                            |> String.join "/"
                            |> (\s -> String.append s "/")
                            |> text
                        )
                , placeholder = Nothing
                , text = model.currentLine
                }

        children =
            if model.inputEnabled then
                lines ++ [ input ]

            else
                lines
    in
    column
        [ width fill
        , height fill
        , padding 4
        , clip
        ]
        children
