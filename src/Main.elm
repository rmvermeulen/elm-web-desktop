module Main exposing (..)

import Browser
import Desktop
import Desktop.Icon
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
    , menu :
        { open : Bool
        }
    }


init : ( Model, Cmd Msg )
init =
    ( { desktop = Desktop.init
      , menu =
            { open = False
            }
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = OpenMenu
    | CloseMenu


update : Msg -> Model -> Model
update msg model =
    case msg of
        OpenMenu ->
            let
                { menu } =
                    model

                openedMenu =
                    { menu | open = True }
            in
            { model | menu = openedMenu }

        CloseMenu ->
            let
                { menu } =
                    model

                closedMenu =
                    { menu | open = False }
            in
            { model | menu = closedMenu }



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ viewDesktopArea model.desktop
        , viewTaskbar model
        ]


viewDesktopArea : Desktop.Model -> Element Msg
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
        , model.icons
            |> List.map Desktop.Icon.view
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


viewTaskbar : Model -> Element Msg
viewTaskbar { menu } =
    let
        menuText =
            "Menu "
                ++ (if menu.open then
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
            if menu.open then
                column
                    [ padding 20
                    , spacing 10
                    , Background.color
                        (rgb 1 1 1)
                    ]
                    [ el [ mouseOver [ Background.color (rgb 0.2 0.2 0.2) ] ] <|
                        text "Option A"
                    , el [ mouseOver [ Background.color (rgb 0.2 0.2 0.2) ] ] <|
                        text "Option B"
                    , el [ mouseOver [ Background.color (rgb 0.2 0.2 0.2) ] ] <|
                        text "Option C"
                    ]

            else
                none
        ]
        [ Input.button
            [ padding 10
            , mouseOver [ Background.color (rgb 0.7 0.7 0.7) ]
            ]
            { label = text menuText
            , onPress =
                if menu.open then
                    Just CloseMenu

                else
                    Just OpenMenu
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
        , update = \msg model -> ( update msg model, Cmd.none )
        , subscriptions = always Sub.none
        }
