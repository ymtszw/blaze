module Igniter.Job exposing (Job(..), JobStack, nextPage)

import PAAPI.Kindle as Kindle


type Job
    = Search Kindle.BrowseNode Kindle.Sort Int String (List String)
    | BrowseNodeLookup Kindle.BrowseNode


type alias JobStack =
    List Job


nextPage : Int -> Job -> Maybe Job
nextPage totalPages job =
    if totalPages <= 1 then
        Nothing
    else
        case job of
            Search _ _ 10 _ _ ->
                Nothing

            Search bn s pa pu kw ->
                Just (Search bn s (pa + 1) pu kw)

            _ ->
                Nothing