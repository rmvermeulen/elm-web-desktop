module Helpers exposing (..)

import Element exposing (..)
import Html.Events
import Json.Decode as Decode


onEnter : msg -> Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )


preventDefaultOnMouseDown : msg -> Attribute msg
preventDefaultOnMouseDown msg =
    htmlAttribute <|
        Html.Events.preventDefaultOn "mousedown"
            (Decode.map (\m -> ( m, True )) (Decode.succeed msg))
