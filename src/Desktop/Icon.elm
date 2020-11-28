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
    }


view : Model -> Element msg
view { name, description, src } =
    column
        [ spacing 10
        , mouseOver
            [ Background.color (rgba 0.3 0.3 0.3 0.3)
            ]
        , Border.rounded 8
        , Border.dotted

        -- , Border.color (rgb 0 0 0)
        , Border.width 2
        , padding 8
        , width fill
        ]
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
