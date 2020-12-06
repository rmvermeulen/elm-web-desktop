module Tests exposing (..)

import App.Terminal as Terminal
import Expect
import Store
import Test exposing (..)


storeModule : Test
storeModule =
    describe "Store module"
        [ test "Store.empty" <|
            \_ ->
                Expect.equal (Store.size Store.empty) 0
        , test "Store.fromList" <|
            \_ ->
                let
                    list =
                        [ 10, 10, 2, 5, 10 ]

                    store =
                        Store.fromList list
                in
                Expect.equal (Store.size store) (List.length list)
        , test "Store.add" <|
            \_ ->
                let
                    ( Store.Id id, store ) =
                        Store.add "Some value" Store.empty
                in
                Expect.equal (Store.size store) (id + 1)
        , test "Store.member (empty)" <|
            \_ ->
                let
                    ( id, _ ) =
                        Store.add "Some value" Store.empty
                in
                Expect.false "Id in empty store" (Store.member id Store.empty)
        , test "Store.member" <|
            \_ ->
                let
                    ( id, store ) =
                        Store.add "Some value" Store.empty
                in
                Expect.true ("Id not in store " ++ Debug.toString store ++ " " ++ Debug.toString id) (Store.member id store)
        , test "Store.replace" <|
            \_ ->
                let
                    ( id, store ) =
                        Store.add "Some value" Store.empty

                    newStore =
                        Store.replace id "Different value" store
                in
                if Store.member id store then
                    Expect.equal (Store.values newStore) [ "Different value" ]

                else
                    Expect.fail "Could not replace id"
        , test "Store.mapSingle" <|
            \_ ->
                let
                    ( id, store ) =
                        Store.add "Some value" Store.empty

                    newStore =
                        Store.mapSingle id (\_ -> "Different value") store
                in
                if Store.member id store then
                    Expect.equal (Store.values newStore) [ "Different value" ]

                else
                    Expect.fail "Could not mapSingle at id"
        , test "Store.values" <|
            \_ ->
                let
                    list =
                        [ 10, 10, 2, 5, 10 ]

                    store =
                        Store.fromList list
                in
                Expect.equal (Store.values store) list
        , test "Store.pairs" <|
            \_ ->
                let
                    list =
                        [ 10, 10, 2, 5, 10 ]

                    store =
                        Store.fromList list

                    expected =
                        list |> List.indexedMap (\i item -> ( Store.Id i, item ))
                in
                Expect.equal (Store.pairs store) expected
        , test "Store.map" <|
            \_ ->
                let
                    ( _, store ) =
                        Store.add "Some value" Store.empty

                    fn k v =
                        v ++ ", and another value"
                in
                Expect.equal (Store.map fn store |> Store.values) [ "Some value, and another value" ]
        ]


terminalApp : Test
terminalApp =
    let
        ( initModel, initCmd ) =
            Terminal.init

        update =
            Terminal.update
    in
    describe "Terminal app"
        [ test "No output at the start" <|
            \_ -> List.isEmpty initModel.lines |> Expect.equal True
        , test "Msg Print" <|
            let
                string =
                    "some text"

                ( model, _ ) =
                    update (Terminal.Print string) initModel
            in
            \_ ->
                model.lines
                    |> Expect.equal [ string ]
        , test "Input is disabled" <|
            \_ -> Expect.false "Input enabled?" initModel.inputEnabled
        , test "Msg EnableInput" <|
            let
                ( model, _ ) =
                    update Terminal.EnableInput initModel
            in
            \_ -> model.inputEnabled |> Expect.true "Input should be enabled"
        , test "Msg DisableInput" <|
            let
                ( model, _ ) =
                    { initModel | inputEnabled = True }
                        |> update Terminal.DisableInput
            in
            \_ -> model.inputEnabled |> Expect.false "Input should be disabled"
        , test "Msg EditLine" <|
            let
                line =
                    "this is the new line"

                ( model, _ ) =
                    update (Terminal.EditLine line) initModel
            in
            \_ -> model.currentLine |> Expect.equal line
        ]
