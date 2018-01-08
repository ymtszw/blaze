port module Fuse exposing (writeFile)

import Set exposing (Set)


-- Ports


port writeFile : ( String, String ) -> Cmd msg
