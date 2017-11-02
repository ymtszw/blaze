module Util exposing (..)


type alias KVS =
    List ( String, String )


(=>) : x -> y -> ( x, y )
(=>) =
    (,)


isOk : Result a b -> Bool
isOk r =
    case r of
        Ok _ ->
            True

        Err _ ->
            False


isErr : Result a b -> Bool
isErr =
    not << isOk
