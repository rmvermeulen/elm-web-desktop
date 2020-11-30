module Desktop exposing (..)

import Basics.Extra
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


type alias Process =
    { app : App
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
            [ createApp "TextEditor"
            , createApp "ImageEditor"
            , createApp "Some Game"
            , createApp "IntegratedTerminal"
            , createApp "Photos"
            , createApp "Some stupidly long name"
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
            Table.add (Process <| createApp "TextEditor") Table.empty
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

        StartProcess program ->
            let
                processes =
                    Table.add (Process program) model.processes
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
                 -- ++ (if model.mFocus /= Nothing then
                 --         [ Events.onClick Deselect ]
                 --     else
                 --         []
                 --    )
                )
                none

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
                            |> minimum 40
                            |> maximum 250
                        )
                    ]
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
                , mouseOver [ Background.color (rgb 0.7 0.7 0.7) ]
                , width (shrink |> minimum 100)
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
                                        [ mouseOver [ Background.color (rgb 0.2 0.2 0.2) ]
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
                        (rgb 1 1 1)
                    ]
                    items

            else
                none
    in
    row
        [ width fill
        , height (fill |> maximum 64)
        , Background.color (rgb 0.4 0.4 0.4)
        , padding 10
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
    text app.name


viewIcon : Icon -> Table.Id Icon -> Bool -> Element Msg
viewIcon { name, description, src } id selected =
    let
        baseAttrs =
            [ spacing 10
            , mouseOver
                [ Background.color (rgba 0.3 0.3 0.3 0.3)
                ]
            , width fill
            , Events.onClick
                (Select <| DesktopIcon id)
            ]

        selectionAttrs =
            if selected then
                [ Border.dotted

                -- , Border.rounded 8
                , Border.width 2
                , padding 8
                , width fill
                , height fill
                , Background.color (rgba 1 1 1 0.3)
                ]

            else
                []

        selectionVisual =
            el selectionAttrs none
                |> behindContent
    in
    column (selectionVisual :: baseAttrs)
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


quickGradient : { angle : Float, stepCount : Int, start : Element.Color, end : Element.Color } -> Attribute Msg
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
