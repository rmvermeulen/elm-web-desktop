module Colors exposing (..)

import Element exposing (Color, rgb, rgba)


red : Color
red =
    rgb 1 0 0


green : Color
green =
    rgb 0 1 0


yellow : Color
yellow =
    rgb 0 1 1


blue : Color
blue =
    rgb 0 0 1


black : Color
black =
    rgb 0 0 0


white : Color
white =
    rgb 1 1 1


gray : Float -> Color
gray n =
    rgb n n n
