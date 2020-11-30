module Table exposing (..)

import Dict exposing (Dict)



-- APPLICATIONS


type Id info
    = Id Int



-- TABLE


type Table info
    = Table Int (Dict Int info)


empty : Table info
empty =
    Table 0 Dict.empty


get : Id info -> Table info -> Maybe info
get (Id id) (Table _ dict) =
    Dict.get id dict


remove : Id info -> Table info -> Table info
remove (Id id) (Table nextId dict) =
    Table nextId (Dict.remove id dict)


add : info -> Table info -> ( Table info, Id info )
add info (Table nextId dict) =
    ( Table (nextId + 1) (Dict.insert nextId info dict)
    , Id nextId
    )


filter : (info -> Bool) -> Table info -> Table info
filter fn (Table nextId dict) =
    Table nextId (Dict.filter (\_ value -> fn value) dict)


replace : Id info -> info -> Table info -> Table info
replace (Id id) info (Table nextId dict) =
    Table nextId (Dict.insert id info dict)


pairs : Table info -> List ( Id info, info )
pairs (Table _ dict) =
    Dict.toList dict |> List.map (Tuple.mapFirst Id)


values : Table info -> List info
values (Table _ dict) =
    Dict.values dict
