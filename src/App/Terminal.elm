module App.Terminal exposing (..)

import Delay
import Element exposing (..)
import Element.Input as Input
import Helpers


type alias Model =
    { inputEnabled : Bool
    , currentLine : String
    , commands : List String
    , lines : List String
    }


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


type Msg
    = EditLine String
    | ExecLine
    | Print String
    | EnableInput
    | DisableInput


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ExecLine ->
            let
                command =
                    model.currentLine

                ( output, execCmd ) =
                    execCommand command
            in
            { model
                | commands = model.commands ++ [ command ]
                , currentLine = ""
            }
                |> update (Print output)
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


execCommand : String -> ( String, Cmd Msg )
execCommand command =
    case command of
        "help" ->
            ( "[ Help ]"
            , [ "help - show this information", "ls - list items in current folder", "" ]
                |> printSequence 25
                |> Cmd.batch
            )

        "ls" ->
            let
                lineCmds =
                    printSequence 25 [ "file1", "file2", "home/", "vacation/" ]
            in
            ( ""
            , Cmd.batch lineCmds
            )

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
