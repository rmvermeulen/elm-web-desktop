module StoreTests exposing (..)

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
