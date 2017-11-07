module Igniter.Model exposing (Model, Mode(..), parseOptions)

import PAAPI
import Igniter.Kindle


type alias Model =
    { paapiCredentials : PAAPI.Credentials
    , options : Options
    , previousResult : Maybe Igniter.Kindle.SearchResult
    , running : Bool
    }


type alias Options =
    { mode : Mode
    , argv : List String
    }


type Mode
    = Search
    | LookupBrowseNode


parseOptions : List String -> Options
parseOptions argv =
    case argv of
        [] ->
            Options Search []

        mode :: tail ->
            if String.toLower mode == "browsenode" then
                Options LookupBrowseNode tail
            else
                Options Search []
