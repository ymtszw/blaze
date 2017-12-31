module Igniter.Model exposing (Model, Options, Mode(..), parseOptions)

import PAAPI
import Igniter.Job exposing (Job, JobStack)


type alias Model =
    { paapiCredentials : PAAPI.Credentials
    , associateTag : PAAPI.AssociateTag
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
    | ItemLookup


parseOptions : List String -> Options
parseOptions argv =
    case argv of
        [] ->
            Options Search []

        mode :: tail ->
            case String.toLower mode of
                "browsenode" ->
                    Options BrowseNodeLookup tail

                "itemlookup" ->
                    Options ItemLookup tail

                _ ->
                    Options Search []
