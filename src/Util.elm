module Util exposing (..)


type alias KVS =
    List ( String, String )


(=>) : x -> y -> ( x, y )
(=>) =
    (,)
