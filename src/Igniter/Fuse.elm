port module Igniter.Fuse exposing (..)


port ignite : (String -> msg) -> Sub msg
