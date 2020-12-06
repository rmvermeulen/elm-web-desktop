module FileSystem exposing (..)


type alias FileInfo =
    { name : String, content : String }


type alias DirInfo =
    { name : String, entries : List Entry }


type Entry
    = File FileInfo
    | Dir DirInfo


dirAtPath : List String -> DirInfo -> Maybe DirInfo
dirAtPath segments root =
    let
        fn segment ( dir, path ) =
            let
                matches =
                    dir.entries
                        |> List.filterMap
                            (\entry ->
                                case entry of
                                    Dir dirInfo ->
                                        if dirInfo.name == segment then
                                            Just dirInfo

                                        else
                                            Nothing

                                    _ ->
                                        Nothing
                            )

                exit =
                    -- cancel further folding by giving empty directory
                    ( DirInfo "/" [], path )
            in
            if List.isEmpty matches then
                exit

            else
                matches
                    |> List.head
                    |> Maybe.map (\newDir -> ( newDir, segment :: path ))
                    |> Maybe.withDefault exit
    in
    segments
        |> List.foldl fn ( root, [] )
        |> Tuple.first
        |> (\info ->
                if info.name == "/" then
                    Nothing

                else
                    Just info
           )
