module Desktop exposing (..)

import Basics.Extra
import Colors exposing (..)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import List.Extra
import Maybe.Extra
import Set exposing (Set)
import Set.Extra
import String.Extra
import Table exposing (Table)


type alias Icon =
    { name : String
    , description : String
    , src : String
    }


type alias App =
    { name : String
    , icon : String
    }


type alias Window =
    { title : String
    , x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type alias Process =
    { app : App
    , window : Window
    }


type Focus
    = TaskbarMenu
    | DesktopIcon (Table.Id Icon)


type alias Model =
    { icons : Table Icon
    , mFocus : Maybe Focus
    , processes : Table Process
    , apps : Table App
    }


type Msg
    = StopProcess (Table.Id Process)
    | StartProcess App
    | ToggleSelect Focus
    | Select Focus
    | Deselect


init : Model
init =
    let
        createApp name =
            App name "logo.svg"

        appList =
            [ createApp "Text Editor"
            , createApp "Image Editor"
            , createApp "Shooter Game"
            , createApp "Terminal"
            , createApp "Photos"
            , createApp "My favorite document(2) - final.jpg"
            ]

        icons : Table Icon
        icons =
            let
                addIcon : App -> Table Icon -> Table Icon
                addIcon app table =
                    let
                        description =
                            "about:" ++ app.name

                        icon =
                            Icon app.name description app.icon
                    in
                    Table.add icon table |> Tuple.first
            in
            appList
                |> List.foldl addIcon Table.empty

        processes : Table Process
        processes =
            let
                process : String -> Process
                process n =
                    Process (createApp n) (Window n 100 100 400 200)
            in
            Table.add (process "TextEditor") Table.empty
                |> Tuple.first

        apps : Table App
        apps =
            let
                addApp prog table =
                    Table.add prog table
                        |> Tuple.first
            in
            appList
                |> List.foldl addApp Table.empty
    in
    Model icons Nothing processes apps


update : Msg -> Model -> Model
update msg model =
    case msg of
        StopProcess id ->
            { model
                | processes = Table.remove id model.processes
            }

        StartProcess app ->
            let
                process =
                    Process app (Window app.name 0 0 100 100)

                processes =
                    Table.add process model.processes
                        |> Tuple.first
            in
            { model
                | processes = processes
            }

        ToggleSelect target ->
            { model
                | mFocus =
                    case model.mFocus of
                        Just focus ->
                            if focus == target then
                                Nothing

                            else
                                Just target

                        Nothing ->
                            Just target
            }

        Select target ->
            { model | mFocus = Just target }

        Deselect ->
            { model | mFocus = Nothing }


view : Model -> Element Msg
view model =
    let
        windows =
            model.processes
                |> Table.values
                |> List.map (.window >> viewWindow)

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
                |> Table.pairs
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


viewProcess : Int -> Process -> Element Msg
viewProcess pid { app } =
    column []
        [ row []
            [ text app.name
            , row [] [ text "close", text "min", text "max" ]
            ]
        ]


viewWindow : Window -> Element Msg
viewWindow { title, x, y, w, h } =
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
                [ Input.button attrs { label = text "-", onPress = Nothing }
                , Input.button attrs { label = text "[]", onPress = Nothing }
                , Input.button attrs { label = text "X", onPress = Nothing }
                ]

        header =
            el [ width fill, height shrink, Background.color blue ] <|
                row [ width fill ]
                    [ el [ width fill ] <| text title
                    , controls
                    ]

        body =
            el
                [ width (px w)
                , height (px h)
                , Background.color (gray 0.8)
                ]
            <|
                text "This is the body"

        footer =
            el [ width (px w), height (px 20), Background.color (gray 0.5) ] <|
                text "This is the footer"

        window =
            column
                [ width shrink, height shrink ]
                [ header
                , body
                , footer
                ]
    in
    el
        [ paddingXY x y
        ]
        window


viewTaskbar : Model -> Element Msg
viewTaskbar { mFocus, apps, processes } =
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
                        apps
                            |> Table.values
                            |> List.map
                                (\{ name } ->
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
                    |> Table.values
                    |> List.map viewTaskbarProcess
               )
        )


viewTaskbarProcess : Process -> Element Msg
viewTaskbarProcess { app } =
    Input.button [ padding 10, Background.color (rgba 1 1 1 0.25) ]
        { label = text app.name
        , onPress = Nothing
        }


viewIcon : Icon -> Table.Id Icon -> Bool -> Element Msg
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
