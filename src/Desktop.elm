module Desktop exposing (..)


type alias Program =
    { name : String }


type alias Process =
    { id : Int
    , program : Program
    }


type alias Model =
    { programs : List Program
    , processes : List Process
    }


type Msg
    = NoOp


init : Model
init =
    { programs = [ Program "TextEditor", Program "ImageEditor" ]
    , processes = []
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
