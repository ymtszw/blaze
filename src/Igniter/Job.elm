module Igniter.Job exposing (Job(..), JobStack, task, nextPage)

import Task exposing (Task)
import PAAPI
import PAAPI.Kindle as Kindle


type Job
    = Search Kindle.BrowseNode Kindle.Sort Int String (List String)
    | BrowseNodeLookup Kindle.BrowseNode
    | ItemLookup (List String)


type alias JobStack =
    List Job


task : PAAPI.Credentials -> PAAPI.AssociateTag -> Job -> Task PAAPI.Error Kindle.Response
task paapiCredentials associateTag job =
    case job of
        Search browseNode sort page publisher keywords ->
            Kindle.search paapiCredentials associateTag browseNode sort page publisher keywords

        BrowseNodeLookup browseNode ->
            Kindle.browseNodeLookup paapiCredentials associateTag browseNode

        ItemLookup asins ->
            Kindle.itemLookup paapiCredentials associateTag asins


nextPage : Int -> Job -> Maybe Job
nextPage totalPages job =
    if totalPages <= 1 then
        Nothing
    else
        case job of
            Search _ _ 10 _ _ ->
                Nothing

            Search bn s pa pu kw ->
                if pa >= totalPages then
                    Nothing
                else
                    Just <| Search bn s (pa + 1) pu kw

            _ ->
                Nothing
