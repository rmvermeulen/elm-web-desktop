module Desktop exposing (..)

import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Set exposing (Set)


type alias Icon =
    { name : String
    , description : String
    , src : String
    }


type alias Program =
    { name : String
    , icon : String
    }


type alias Process =
    { id : Int
    , program : Program
    }


type Focus
    = TaskbarMenu
    | DesktopIcon String


type alias Model =
    { icons : Dict String Icon
    , mFocus : Maybe Focus
    , nextId : Int
    , processes : List Process
    , programs : Dict String Program
    }


type Msg
    = StopProcess Int
    | StartProcess Program
    | ToggleSelect Focus
    | Select Focus
    | Deselect


init : Model
init =
    let
        programList =
            [ Program "TextEditor" "logo.svg"
            , Program "ImageEditor" "logo.svg"
            , Program "Some Game" "logo.svg"
            , Program "IntegratedTerminal" "logo.svg"
            , Program "Photos" "logo.svg"
            , Program "Some stupidly long name" "logo.svg"
            ]

        icons =
            programList
                |> List.map
                    (\{ name } ->
                        let
                            description =
                                "about:" ++ name
                        in
                        ( name, Icon name description "logo.svg" )
                    )
                |> Dict.fromList

        nextId =
            1001

        processes =
            [ Process 1000 (Program "TextEditor" "logo.svg") ]

        programs =
            programList
                |> List.map (\prog -> ( prog.name, prog ))
                |> Dict.fromList
    in
    Model icons Nothing nextId processes programs


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
    column [ width fill, height fill ]
        [ el
            [ width fill
            , height fill

            -- , Events.onClick Deselect
            , quickGradient
                { angle = 0.05 * pi
                , stepCount = 32
                , start = rgb 0.2 0 0.4
                , end = rgb 0.6 0.6 1
                }
            , model.icons
                |> Dict.map
                    (\key icon ->
                        let
                            selected =
                                case model.mFocus of
                                    Just (DesktopIcon name) ->
                                        name == key

                                    _ ->
                                        False
                        in
                        viewIcon icon selected
                    )
                |> Dict.values
                |> column
                    [ padding 20
                    , spacing 10
                    , width
                        (shrink
                            |> minimum 40
                            |> maximum 250
                        )
                    ]
                |> inFront
            ]
            none
        , viewTaskbar model
        ]


viewProcess : Process -> Element Msg
viewProcess { id, program } =
    column []
        [ row []
            [ text program.name
            , row [] [ text "close", text "min", text "max" ]
            ]
        ]


viewTaskbar : Model -> Element Msg
viewTaskbar { mFocus, programs, processes } =
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
    in
    row
        [ width fill
        , height (fill |> maximum 64)
        , Background.color (rgb 0.4 0.4 0.4)
        , padding 10
        , above <|
            if selected then
                let
                    items =
                        programs
                            |> Dict.values
                            |> List.map
                                (\{ name } ->
                                    el [ mouseOver [ Background.color (rgb 0.2 0.2 0.2) ] ] <|
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
        ]
    <|
        Input.button
            [ padding 10
            , mouseOver [ Background.color (rgb 0.7 0.7 0.7) ]
            ]
            { label = text menuText
            , onPress =
                Just <| ToggleSelect TaskbarMenu
            }
            :: (processes
                    |> List.map viewTaskbarProcess
               )


viewTaskbarProcess : Process -> Element Msg
viewTaskbarProcess { id, program } =
    text <| program.name ++ "@" ++ String.fromInt id


viewIcon : Icon -> Bool -> Element Msg
viewIcon { name, description, src } selected =
    let
        borderAttrs =
            if selected then
                [ Border.rounded 8
                , Border.dotted
                , Border.width 2
                , padding 8
                ]

            else
                [ Events.onClick (Select <| DesktopIcon name) ]
    in
    column
        (borderAttrs
            ++ [ spacing 10
               , mouseOver
                    [ Background.color (rgba 0.3 0.3 0.3 0.3)
                    ]
               , width fill
               ]
        )
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
