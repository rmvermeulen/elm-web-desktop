module App.Game exposing (..)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input


type alias Model =
    { lives : Int
    , score : Int
    }


init : Model
init =
    Model 0 0


type Msg
    = Increment


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | score = model.score + 1 }, Cmd.none )


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ text "[App.Game]"
        , text <| "Score: " ++ String.fromInt model.score
        , Input.button [ padding 4, Border.width 1, Border.rounded 8 ]
            { label = text "Increment"
            , onPress = Just Increment
            }
        ]
