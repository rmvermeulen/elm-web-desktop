module Store exposing (..)

import Dict exposing (Dict)


type Id data
    = Id Int


type Store data
    = Store Int (Dict Int data)


fromList : List data -> Store data
fromList list =
    let
        addRec : List data -> Int -> Dict Int data -> Dict Int data
        addRec items id dict =
            case items of
                first :: rest ->
                    addRec rest (id + 1) (Dict.insert id first dict)

                _ ->
                    dict
    in
    Store (List.length list) (addRec list 0 Dict.empty)


empty : Store data
empty =
    Store 0 Dict.empty


add : data -> Store data -> ( Id data, Store data )
add data (Store id dict) =
    let
        newDict =
            Dict.insert id data dict
    in
    ( Id id, Store (id + 1) newDict )


get : Id data -> Store data -> Maybe data
get (Id id) (Store _ dict) =
    Dict.get id dict


remove : Id info -> Store info -> Store info
remove (Id id) (Store nextId dict) =
    Store nextId (Dict.remove id dict)


replace : Id data -> data -> Store data -> Store data
replace (Id id) data (Store storeId dict) =
    if Dict.member id dict then
        Store storeId (Dict.insert id data dict)

    else
        Store storeId dict


member : Id data -> Store data -> Bool
member (Id id) (Store _ dict) =
    Dict.member id dict


size : Store data -> Int
size (Store _ dict) =
    Dict.size dict


values : Store data -> List data
values (Store _ dict) =
    Dict.values dict


pairs : Store data -> List ( Id data, data )
pairs (Store _ dict) =
    Dict.toList dict
        |> List.map (Tuple.mapFirst Id)


map : (Id data -> data -> data) -> Store data -> Store data
map fn (Store storeId storeDict) =
    let
        mapper id value dict =
            Dict.insert id (fn (Id id) value) dict

        newDict =
            Dict.foldl mapper Dict.empty storeDict
    in
    Store storeId newDict


mapSingle : Id data -> (data -> data) -> Store data -> Store data
mapSingle (Id id) fn (Store storeId storeDict) =
    Dict.get id storeDict
        |> Maybe.map (\value -> Dict.insert id (fn value) storeDict)
        |> Maybe.withDefault storeDict
        |> Store storeId
