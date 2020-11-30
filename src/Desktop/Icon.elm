module Desktop.Icon exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font


type alias Model =
    { name : String
    , description : String
    , src : String
    , selected : Bool
    }


view : msg -> Model -> Element msg
view onSelect { name, description, selected, src } =
    let
        borderAttrs =
            if selected then
                [ Border.rounded 8
                , Border.dotted
                , Border.width 2
                , padding 8
                ]

            else
                []
    in
    column
        (borderAttrs
            ++ [ spacing 10
               , mouseOver
                    [ Background.color (rgba 0.3 0.3 0.3 0.3)
                    ]
               , width fill
               , Events.onClick onSelect
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
