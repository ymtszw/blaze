module Igniter.Model exposing (Model, Options, Mode(..), parseOptions)

import PAAPI
import Igniter.Job exposing (Job, JobStack)


type alias Model =
    { paapiCredentials : PAAPI.Credentials
    , options : Options
    , rateLimited : Bool
    , jobStack : JobStack
    , runningJob : Maybe Job
    }


type alias Options =
    { mode : Mode
    , argv : List String
    }


type Mode
    = Search
    | BrowseNodeLookup


parseOptions : List String -> Options
parseOptions argv =
    case argv of
        [] ->
            Options Search []

        mode :: tail ->
            if String.toLower mode == "browsenode" then
                Options BrowseNodeLookup tail
            else
                Options Search []
