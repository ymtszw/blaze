port module Igniter.Fuse exposing (..)

import Igniter.Model exposing (Model)


port sendModelDump : Model -> Cmd msg


port requestModelDump : (() -> msg) -> Sub msg
