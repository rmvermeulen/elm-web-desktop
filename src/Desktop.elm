module Desktop exposing (..)

import Desktop.Icon as Icon
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Set exposing (Set)


type alias Program =
    { name : String
    , icon : String
    }


type alias Process =
    { id : Int
    , program : Program
    }


type alias Model =
    { icons : Dict String Icon.Model
    , nextId : Int
    , processes : List Process
    , programs : Dict String Program
    }


type Msg
    = StopProcess Int
    | StartProcess Program
    | Select (Set String)
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
    in
    { icons =
        programList
            |> List.map
                (\{ name } ->
                    let
                        description =
                            "about:" ++ name
                    in
                    ( name, Icon.Model name description "logo.svg" False )
                )
            |> Dict.fromList
    , nextId = 1001
    , processes = [ Process 1000 (Program "TextEditor" "logo.svg") ]
    , programs =
        programList
            |> List.map (\prog -> ( prog.name, prog ))
            |> Dict.fromList
    }


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

        Select selected ->
            let
                icons =
                    model.icons
                        |> Dict.map
                            (\key icon ->
                                { icon | selected = Set.member key selected }
                            )
            in
            { model | icons = icons }

        Deselect ->
            let
                deselected =
                    model.icons
                        |> Dict.map
                            (\_ icon -> { icon | selected = False })
            in
            { model | icons = deselected }


view : Model -> Element Msg
view { icons } =
    el
        [ width fill
        , height fill
        , quickGradient
            { angle = 0.05 * pi
            , stepCount = 32
            , start = rgb 0.2 0 0.4
            , end = rgb 0.6 0.6 1
            }
        , icons
            |> Dict.map
                (\key icon ->
                    let
                        onSelect =
                            Select (Set.singleton key)
                    in
                    Icon.view onSelect icon
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
