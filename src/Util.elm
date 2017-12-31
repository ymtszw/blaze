module Util exposing (..)

import Date


type alias KVS =
    List ( String, String )


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


ymd : Date.Date -> String
ymd date =
    (toString <| Date.year date)
        ++ "/"
        ++ (toString <| month date)
        ++ "/"
        ++ (toString <| Date.day date)


month : Date.Date -> Int
month date =
    case Date.month date of
        Date.Jan ->
            1

        Date.Feb ->
            2

        Date.Mar ->
            3

        Date.Apr ->
            4

        Date.May ->
            5

        Date.Jun ->
            6

        Date.Jul ->
            7

        Date.Aug ->
            8

        Date.Sep ->
            9

        Date.Oct ->
            10

        Date.Nov ->
            11

        Date.Dec ->
            12
