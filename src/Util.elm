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
        ++ (toString <| monthToInt <| Date.month date)
        ++ "/"
        ++ (toString <| Date.day date)


monthToInt : Date.Month -> Int
monthToInt month =
    case month of
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


intToMonth : Int -> Date.Month
intToMonth int =
    case int % 12 of
        1 ->
            Date.Jan

        2 ->
            Date.Feb

        3 ->
            Date.Mar

        4 ->
            Date.Apr

        5 ->
            Date.May

        6 ->
            Date.Jun

        7 ->
            Date.Jul

        8 ->
            Date.Aug

        9 ->
            Date.Sep

        10 ->
            Date.Oct

        11 ->
            Date.Nov

        _ ->
            Date.Dec
