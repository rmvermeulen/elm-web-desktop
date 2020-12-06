module FileSystem exposing (..)


type alias FileInfo =
    { name : String, content : String }


type alias DirInfo =
    { name : String, entries : List Entry }


type Entry
    = File FileInfo
    | Dir DirInfo
