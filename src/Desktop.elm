module Desktop exposing (..)

import App.Game as Game
import App.ImageEditor as ImageEditor
import App.Terminal as Terminal
import App.TextEditor as TextEditor
import Basics.Extra
import Browser.Events
import Colors exposing (..)
import Delay
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import FileSystem as Fs
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Set.Extra
import Store exposing (Store)
import String.Extra
import Task


type alias Icon =
    { app : App
    , name : String
    , description : String
    , src : String
    }


type WindowState
    = Floating
    | Maximized


type alias Window =
    { title : String
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    , minimized : Bool
    , state : WindowState
    , depth : Int
    }


createWindow : String -> Int -> Int -> Window
createWindow title x y =
    Window title x y 300 200 False Floating 0


type alias Process =
    { window : Window
    , data : AppData
    }


type Focus
    = TaskbarMenu
    | DesktopIcon IconId


type App
    = TextEditor
    | ImageEditor
    | Terminal
    | Game


type AppData
    = TextEditorData TextEditor.Model
    | ImageEditorData ImageEditor.Model
    | TerminalData Terminal.Model
    | GameData Game.Model


type DragState
    = DragStart Pid
    | DragMove Pid Int Int


type alias Model =
    { icons : Store Icon
    , processes : Store Process
    , mFocus : Maybe Focus
    , mDragState : Maybe DragState
    , mActivatedIcon : Maybe IconId
    , fs : Fs.DirInfo
    }


type alias AppId =
    Store.Id App


type alias IconId =
    Store.Id Icon


type alias Pid =
    Store.Id Process


type Msg
    = StopProcess Pid
    | StartProcess App
    | ToggleSelect Focus
    | Select Focus
    | Deselect
    | MinimizeWindow Pid
    | UnMinimizeWindow Pid
    | MaximizeWindow Pid
    | UnMaximizeWindow Pid
    | ResizeWindow Pid Int Int
    | StartDragWindow Pid
    | SetDragOffset Int Int
    | StopDragWindow
    | MoveDragWindow Int Int
    | DeactivateIcon
    | GameMsg Pid Game.Msg
    | TextEditorMsg Pid TextEditor.Msg
    | ImageEditorMsg Pid ImageEditor.Msg
    | TerminalMsg Pid Terminal.Msg


init : ( Model, Cmd Msg )
init =
    let
        icons : Store Icon
        icons =
            let
                getInfo : App -> ( String, String )
                getInfo app =
                    ( appName app, appIconPath app )

                addIcon : App -> Store Icon -> Store Icon
                addIcon app table =
                    let
                        ( name, iconPath ) =
                            getInfo app

                        description =
                            "about:" ++ name

                        icon : Icon
                        icon =
                            Icon app name description iconPath
                    in
                    Store.add icon table |> Tuple.second
            in
            [ TextEditor, ImageEditor, Terminal, Game ]
                |> List.foldl addIcon Store.empty

        processes : Store Process
        processes =
            Store.empty

        fs =
            Fs.DirInfo "/"
                [ Fs.File <| Fs.FileInfo "TextEditor" "[StartProcess:TextEditor]"
                , Fs.File <| Fs.FileInfo "ImageEditor" "[StartProcess:ImageEditor]"
                , Fs.File <| Fs.FileInfo "Terminal" "[StartProcess:Terminal]"
                , Fs.File <| Fs.FileInfo "Game" "[StartProcess:Game]"
                , Fs.Dir <| Fs.DirInfo "photos" []
                ]
    in
    ( Model icons processes Nothing Nothing Nothing fs
    , Cmd.none
    )


appDataType : AppData -> App
appDataType data =
    case data of
        TextEditorData _ ->
            TextEditor

        ImageEditorData _ ->
            ImageEditor

        TerminalData _ ->
            Terminal

        GameData _ ->
            Game


appIconPath : App -> String
appIconPath app =
    case app of
        TextEditor ->
            "logo.svg"

        ImageEditor ->
            "logo.svg"

        Terminal ->
            "logo.svg"

        Game ->
            "logo.svg"


processName : Process -> String
processName =
    .data >> appDataType >> appName


appName : App -> String
appName app =
    case app of
        TextEditor ->
            "TextEditor"

        ImageEditor ->
            "ImageEditor"

        Terminal ->
            "Terminal"

        Game ->
            "Game"


selectTarget : Focus -> Model -> ( Model, Cmd Msg )
selectTarget target model =
    let
        delayedDeactivation =
            Delay.after 350 Delay.Millisecond DeactivateIcon

        withDelayedDeactivation m =
            ( m, delayedDeactivation )
    in
    case target of
        TaskbarMenu ->
            -- select taskbar, deselect and deactivate icons
            withDelayedDeactivation
                { model
                    | mFocus = Just target
                    , mActivatedIcon = Nothing
                }

        DesktopIcon targetIcon ->
            -- select icon, update active icon, possible start process
            let
                acceptedTarget =
                    withDelayedDeactivation
                        { model
                            | mFocus = Just target
                            , mActivatedIcon = Just targetIcon
                        }
            in
            case model.mActivatedIcon of
                Nothing ->
                    acceptedTarget

                Just activeIcon ->
                    if activeIcon == targetIcon then
                        -- double click same icon: activate icon
                        Store.get targetIcon model.icons
                            |> Maybe.map
                                (\{ app } ->
                                    let
                                        ( m, c ) =
                                            update (StartProcess app) model
                                    in
                                    ( m, Cmd.batch [ c, delayedDeactivation ] )
                                )
                            |> Maybe.withDefault acceptedTarget

                    else
                        acceptedTarget


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        simply m =
            ( m, Cmd.none )
    in
    case msg of
        StopProcess id ->
            simply
                { model
                    | processes = Store.remove id model.processes
                }

        StartProcess app ->
            let
                ( newProcess, createCmd ) =
                    let
                        title =
                            appName app

                        window =
                            createWindow title 100 100
                    in
                    case app of
                        TextEditor ->
                            TextEditor.init
                                |> Tuple.mapBoth
                                    (\data -> TextEditorData data |> Process window)
                                    (\cmd -> \pid -> Cmd.map (TextEditorMsg pid) cmd)

                        ImageEditor ->
                            ImageEditor.init
                                |> Tuple.mapBoth
                                    (\data -> ImageEditorData data |> Process window)
                                    (\cmd -> \pid -> Cmd.map (ImageEditorMsg pid) cmd)

                        Terminal ->
                            Terminal.init
                                |> Tuple.mapBoth
                                    (\data -> TerminalData data |> Process window)
                                    (\cmd -> \pid -> Cmd.map (TerminalMsg pid) cmd)

                        Game ->
                            Game.init
                                |> Tuple.mapBoth
                                    (\data -> GameData data |> Process window)
                                    (\cmd -> \pid -> Cmd.map (GameMsg pid) cmd)

                ( newPid, processes ) =
                    Store.add newProcess model.processes
            in
            ( { model | processes = processes }, createCmd newPid )

        ToggleSelect target ->
            case model.mFocus of
                Just focus ->
                    if focus == target then
                        -- select current target again, so deselect
                        update Deselect model

                    else
                        -- different selection, select a new target
                        update (Select target) model

                Nothing ->
                    -- no current selection, select new target
                    update (Select target) model

        Select target ->
            selectTarget target model

        Deselect ->
            simply { model | mFocus = Nothing, mActivatedIcon = Nothing }

        MinimizeWindow id ->
            let
                processes =
                    updateProcessWindow id (\window -> { window | minimized = True }) model.processes
            in
            simply { model | processes = processes }

        UnMinimizeWindow id ->
            let
                processes =
                    updateProcessWindow id (\window -> { window | minimized = False }) model.processes
            in
            simply { model | processes = processes }

        MaximizeWindow id ->
            let
                processes =
                    updateProcessWindow id (\window -> { window | state = Maximized }) model.processes
            in
            simply { model | processes = processes }

        UnMaximizeWindow id ->
            let
                processes =
                    updateProcessWindow id (\window -> { window | state = Floating }) model.processes
            in
            simply { model | processes = processes }

        ResizeWindow id w h ->
            let
                processes =
                    updateProcessWindow id
                        (\window ->
                            { window
                                | w = w
                                , h = h
                            }
                        )
                        model.processes
            in
            simply { model | processes = processes }

        StartDragWindow pid ->
            let
                reorderedProcesses =
                    model.processes
                        |> Store.map
                            (\id data ->
                                let
                                    { window } =
                                        data

                                    depth =
                                        if id == pid then
                                            0

                                        else
                                            window.depth + 1
                                in
                                { data | window = { window | depth = depth } }
                            )
            in
            simply
                { model
                    | mDragState = Just (DragStart pid)
                    , processes = reorderedProcesses
                }

        StopDragWindow ->
            simply { model | mDragState = Nothing }

        SetDragOffset x y ->
            let
                dragState =
                    model.mDragState
                        |> Maybe.map
                            (\state ->
                                case state of
                                    DragStart pid ->
                                        DragMove pid x y

                                    DragMove pid _ _ ->
                                        DragMove pid x y
                            )
            in
            simply { model | mDragState = dragState }

        MoveDragWindow x y ->
            case model.mDragState of
                Just (DragMove id offsetX offsetY) ->
                    let
                        processes =
                            updateProcessWindow id
                                (\window ->
                                    { window
                                        | x = x + offsetX
                                        , y = y + offsetY
                                    }
                                )
                                model.processes
                    in
                    simply { model | processes = processes }

                Just (DragStart _) ->
                    simply model

                Nothing ->
                    simply model

        DeactivateIcon ->
            simply { model | mActivatedIcon = Nothing }

        GameMsg pid gameMsg ->
            model.processes
                |> Store.get pid
                |> Maybe.map
                    (\proc ->
                        let
                            ( data, cmd ) =
                                case proc.data of
                                    GameData gameModel ->
                                        let
                                            ( newGameModel, gameCmd ) =
                                                Game.update gameMsg gameModel
                                        in
                                        ( GameData newGameModel, gameCmd |> Cmd.map (GameMsg pid) )

                                    _ ->
                                        ( proc.data, Cmd.none )

                            newProc =
                                { proc | data = data }

                            processes =
                                Store.replace pid newProc model.processes
                        in
                        ( { model | processes = processes }, cmd )
                    )
                |> Maybe.withDefault (simply model)

        TextEditorMsg pid textEditorMsg ->
            model.processes
                |> Store.get pid
                |> Maybe.map
                    (\proc ->
                        let
                            ( data, cmd ) =
                                case proc.data of
                                    TextEditorData textEditorModel ->
                                        let
                                            ( newTextEditorModel, textEditorCmd ) =
                                                TextEditor.update textEditorMsg textEditorModel
                                        in
                                        ( TextEditorData newTextEditorModel, textEditorCmd |> Cmd.map (TextEditorMsg pid) )

                                    _ ->
                                        ( proc.data, Cmd.none )

                            newProc =
                                { proc | data = data }

                            processes =
                                Store.replace pid newProc model.processes
                        in
                        ( { model | processes = processes }, cmd )
                    )
                |> Maybe.withDefault (simply model)

        ImageEditorMsg pid imageEditorMsg ->
            model.processes
                |> Store.get pid
                |> Maybe.map
                    (\proc ->
                        let
                            ( data, cmd ) =
                                case proc.data of
                                    ImageEditorData imageEditorModel ->
                                        let
                                            ( newImageEditorModel, imageEditorCmd ) =
                                                ImageEditor.update imageEditorMsg imageEditorModel
                                        in
                                        ( ImageEditorData newImageEditorModel, imageEditorCmd |> Cmd.map (ImageEditorMsg pid) )

                                    _ ->
                                        ( proc.data, Cmd.none )

                            newProc =
                                { proc | data = data }

                            processes =
                                Store.replace pid newProc model.processes
                        in
                        ( { model | processes = processes }, cmd )
                    )
                |> Maybe.withDefault (simply model)

        TerminalMsg pid terminalMsg ->
            model.processes
                |> Store.get pid
                |> Maybe.map
                    (\proc ->
                        let
                            processInfoList : List Terminal.ProcessInfo
                            processInfoList =
                                Store.pairs model.processes
                                    |> List.map
                                        (\( Store.Id id, p ) ->
                                            Terminal.ProcessInfo id (processName p)
                                        )

                            ( data, cmd ) =
                                case proc.data of
                                    TerminalData terminalModel ->
                                        let
                                            ( newTerminalModel, terminalCmd ) =
                                                Terminal.update processInfoList model.fs terminalMsg terminalModel
                                        in
                                        ( TerminalData newTerminalModel, terminalCmd |> Cmd.map (TerminalMsg pid) )

                                    _ ->
                                        ( proc.data, Cmd.none )

                            newProc =
                                { proc | data = data }

                            processes =
                                Store.replace pid newProc model.processes
                        in
                        ( { model | processes = processes }, cmd )
                    )
                |> Maybe.withDefault (simply model)


updateProcessWindow : Pid -> (Window -> Window) -> Store Process -> Store Process
updateProcessWindow id fn table =
    Store.get id table
        |> Maybe.map
            (\process -> { process | window = fn process.window })
        |> Maybe.map (\proc -> Store.replace id proc table)
        |> Maybe.withDefault table


view : Model -> Element Msg
view model =
    let
        windows =
            model.processes
                |> Store.pairs
                |> List.sortBy (Tuple.second >> .window >> .depth)
                |> List.reverse
                |> List.map (\( id, proc ) -> viewProcessWindow id proc)

        icons =
            let
                renderIcon ( key, icon ) =
                    let
                        selected : Bool
                        selected =
                            case model.mFocus of
                                Just (DesktopIcon name) ->
                                    name == key

                                _ ->
                                    False
                    in
                    viewIcon icon key selected
            in
            model.icons
                |> Store.pairs
                |> List.map renderIcon
                |> column
                    [ padding 20
                    , spacing 10
                    , width
                        (shrink
                            |> minimum 100
                            |> maximum 200
                        )
                    ]

        background =
            el
                ([ width fill
                 , height fill
                 , quickGradient
                    { angle = 0.05 * pi
                    , stepCount = 32
                    , start = rgb 0.2 0 0.4
                    , end = rgb 0.6 0.6 1
                    }
                 , icons |> inFront
                 ]
                    ++ (windows |> List.map inFront)
                 -- ++ (if model.mFocus /= Nothing then
                 --         [ Events.onClick Deselect ]
                 --     else
                 --         []
                 --    )
                )
                none
    in
    column [ width fill, height fill ]
        [ background
        , viewTaskbar model
        ]


viewApp : Pid -> AppData -> Element Msg
viewApp pid data =
    el
        [ width fill
        , height fill
        , Background.color (gray 0.8)
        ]
    <|
        case data of
            GameData model ->
                Game.view model |> map (GameMsg pid)

            ImageEditorData model ->
                ImageEditor.view model |> map (ImageEditorMsg pid)

            TerminalData model ->
                Terminal.view model |> map (TerminalMsg pid)

            TextEditorData model ->
                TextEditor.view model |> map (TextEditorMsg pid)


viewProcessWindow : Pid -> Process -> Element Msg
viewProcessWindow pid { data, window } =
    let
        { title, x, y, w, h, minimized, state } =
            window
    in
    if minimized then
        none

    else
        let
            controls =
                let
                    attrs =
                        [ width (px 20)
                        , height (px 20)
                        , Background.color (gray 0.6)
                        , Border.rounded 8
                        ]
                in
                row [ width shrink, height (shrink |> minimum 25), spacing 2, padding 1 ]
                    [ Input.button attrs
                        { label = text "-"
                        , onPress =
                            if minimized then
                                Just (UnMinimizeWindow pid)

                            else
                                Just (MinimizeWindow pid)
                        }
                    , Input.button attrs
                        { label = text "[]"
                        , onPress =
                            case state of
                                Floating ->
                                    Just (MaximizeWindow pid)

                                Maximized ->
                                    Just (UnMaximizeWindow pid)
                        }
                    , Input.button attrs { label = text "X", onPress = Just (StopProcess pid) }
                    ]

            header =
                el
                    [ width fill
                    , height shrink
                    , Background.color (rgb 0 0 1)
                    , mouseDown [ Background.color (rgb 0.2 0.2 1) ]
                    , Events.onMouseDown (StartDragWindow pid)
                    ]
                <|
                    row [ width fill ]
                        [ el [ width fill ] <| text title
                        , controls
                        ]

            body =
                viewApp pid data

            footer =
                el [ width fill, height (px 20), Background.color (gray 0.5) ] <|
                    text "This is the footer"
        in
        case state of
            Floating ->
                column
                    [ moveRight (toFloat x)
                    , moveDown (toFloat y)
                    , width (px w)
                    , height (px h)
                    ]
                    [ header
                    , body
                    , footer
                    ]

            Maximized ->
                column
                    [ width fill
                    , height fill
                    ]
                    [ header
                    , body
                    , footer
                    ]


viewTaskbar : Model -> Element Msg
viewTaskbar { mFocus, processes } =
    let
        selected =
            case mFocus of
                Just TaskbarMenu ->
                    True

                _ ->
                    False

        menuText =
            "Menu "
                ++ (if selected then
                        "(-)"

                    else
                        "(+)"
                   )

        menuButton =
            Input.button
                [ padding 10
                , mouseOver [ Background.color (gray 0.7) ]
                , width (shrink |> minimum 100)
                , Border.solid
                , Border.width 1
                ]
                { label = text menuText
                , onPress =
                    Just <| ToggleSelect TaskbarMenu
                }

        menu =
            if selected then
                let
                    items =
                        [ TextEditor, ImageEditor, Terminal, Game ]
                            |> List.map appName
                            |> List.map
                                (\name ->
                                    el
                                        [ mouseOver [ Background.color (gray 0.2) ]
                                        , padding 10
                                        , width fill
                                        , Font.alignLeft
                                        ]
                                    <|
                                        text name
                                )
                in
                column
                    [ padding 20
                    , spacing 10
                    , Background.color
                        white
                    ]
                    items

            else
                none
    in
    row
        [ width fill
        , height (fill |> maximum 64)
        , Background.color (gray 0.4)
        , padding 10
        , spacing 8
        , above menu
        ]
        (menuButton
            :: (processes
                    |> Store.pairs
                    |> List.map (\( id, proc ) -> viewTaskbarProcess id proc)
               )
        )


viewTaskbarProcess : Pid -> Process -> Element Msg
viewTaskbarProcess id { data } =
    Input.button [ padding 10, Background.color (rgba 1 1 1 0.25) ]
        { label =
            data
                |> appDataType
                |> appName
                |> text
        , onPress = Just (UnMinimizeWindow id)
        }


viewIcon : Icon -> IconId -> Bool -> Element Msg
viewIcon { name, description, src } id selected =
    let
        baseAttrs =
            [ spacing 10
            , mouseOver
                [ Background.color (gray 0.3)
                ]
            , width fill
            , Events.onClick
                (Select <| DesktopIcon id)
            ]

        selectionAttrs =
            if selected then
                [ Border.dotted
                , Border.width 2
                , width fill
                , height fill
                , Background.color (rgba 1 1 1 0.3)
                ]

            else
                []

        selectionOutline =
            el selectionAttrs none
                |> behindContent
    in
    column (selectionOutline :: baseAttrs)
        [ image [ width (px 40), height (px 40), centerX ]
            { src = src
            , description = description
            }
        , el
            [ width fill
            , Font.center
            ]
            (paragraph [] [ text name ])
        ]


quickGradient : { angle : Float, stepCount : Int, start : Color, end : Color } -> Attribute Msg
quickGradient { angle, stepCount, start, end } =
    let
        ( dr, dg, db ) =
            let
                a =
                    toRgb start

                b =
                    toRgb end

                f =
                    toFloat stepCount
            in
            ( (b.red - a.red) / f
            , (b.green - a.green) / f
            , (b.blue - a.blue) / f
            )

        steps =
            List.repeat stepCount (toRgb start)
                |> List.indexedMap
                    (\index { red, green, blue } ->
                        let
                            s =
                                toFloat index
                        in
                        rgb (red + s * dr) (green + s * dg) (blue + s * db)
                    )
    in
    Background.gradient
        { angle = angle
        , steps = steps
        }


subscriptions : Model -> Sub Msg
subscriptions { mDragState, processes } =
    case mDragState of
        Just (DragMove pid x y) ->
            let
                decoder : Decoder Msg
                decoder =
                    Decode.succeed MoveDragWindow
                        |> required "x" Decode.int
                        |> required "y" Decode.int
            in
            Sub.batch
                [ Browser.Events.onMouseMove decoder
                , Browser.Events.onMouseUp (Decode.succeed StopDragWindow)
                ]

        Just (DragStart pid) ->
            Store.get pid processes
                |> Maybe.map
                    (\{ window } ->
                        let
                            windowOffset x y =
                                SetDragOffset (window.x - x) (window.y - y)

                            decoder : Decoder Msg
                            decoder =
                                Decode.succeed windowOffset
                                    |> required "x" Decode.int
                                    |> required "y" Decode.int
                        in
                        Sub.batch
                            [ Browser.Events.onMouseMove decoder
                            , Browser.Events.onMouseUp (Decode.succeed StopDragWindow)
                            ]
                    )
                |> Maybe.withDefault Sub.none

        Nothing ->
            Sub.none
