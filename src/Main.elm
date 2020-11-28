module Main exposing (..)

import Browser
import Desktop
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Maybe
import Set exposing (Set)



---- MODEL ----


type alias Model =
    { desktop : Desktop.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { desktop = Desktop.init
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ viewDesktopArea model
        , viewTaskbar model
        ]


viewDesktopArea : Model -> Element Msg
viewDesktopArea model =
    el
        [ width fill
        , height fill
        , quickGradient
            { angle = 0.05 * pi
            , stepCount = 32
            , start = rgb 0.2 0 0.4
            , end = rgb 0.6 0.6 1
            }
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


viewTaskbar : Model -> Element Msg
viewTaskbar model =
    row
        [ width fill
        , height (fill |> maximum 64)
        , Background.color (rgb 0.4 0.4 0.4)
        , padding 10
        ]
        [ Input.button
            [ padding 10
            , mouseOver [ Background.color (rgb 0.7 0.7 0.7) ]
            ]
            { label = text "Menu"
            , onPress = Just NoOp
            }
        ]



---- PROGRAM ----
-- state serialized as json


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.element
        { view = \model -> layout [] (view model)
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
