module Main exposing (..)

import Browser
import Desktop
import Desktop.Icon
import Dict exposing (Dict)
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
    | DesktopMsg Desktop.Msg


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

        DesktopMsg desktopMsg ->
            { model
                | desktop =
                    Desktop.update desktopMsg model.desktop
            }



---- VIEW ----


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ Desktop.view model.desktop
            |> map DesktopMsg
        , viewTaskbar model
        ]


viewTaskbar : Model -> Element Msg
viewTaskbar { desktop, menu } =
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
                let
                    items =
                        desktop.programs
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
                if menu.open then
                    Just CloseMenu

                else
                    Just OpenMenu
            }
            :: (desktop.processes
                    |> List.map (\{ id, program } -> text <| "[" ++ String.fromInt id ++ ":" ++ program.name ++ "]")
               )



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
