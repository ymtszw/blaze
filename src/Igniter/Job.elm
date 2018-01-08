module Igniter.Job exposing (Job(..), JobStack, initStack, updateStack, exec)

import Set exposing (Set)
import Task exposing (Task)
import PAAPI
import PAAPI.Kindle as Kindle
import Logger as L
import Igniter.Seed as Seed


-- Types


type Job
    = Search Kindle.BrowseNode Kindle.Sort Int (List String)
    | BrowseNodeLookup Kindle.BrowseNode
    | ItemLookup (List String)
    | CollectPublishers (List Kindle.BrowseNode) Seed.Pubdate Int (Set String)


type alias JobStack =
    List Job



-- Job Initizlization


initStack : List String -> JobStack
initStack argv =
    case argv of
        [] ->
            [ Search Kindle.Boys Kindle.DateRank 1 <| Seed.power (Just Seed.pubdateOrigin) (Seed.By Seed.shueisha) ]

        mode :: tail ->
            case String.toLower mode of
                "browsenode" ->
                    [ BrowseNodeLookup <| targetBrowseNode tail ]

                "itemlookup" ->
                    [ ItemLookup tail ]

                "collectpublishers" ->
                    [ CollectPublishers Seed.comicNodes Seed.pubdateOrigin 1 Seed.wellknownPublishers ]

                _ ->
                    [ Search Kindle.Boys Kindle.DateRank 1 tail ]


targetBrowseNode : List String -> Kindle.BrowseNode
targetBrowseNode argv =
    case argv of
        [] ->
            Kindle.Root

        str :: _ ->
            Kindle.toBrowseNode str



-- Job Update


updateStack :
    JobStack
    -> Maybe Job
    -> { x | totalPages : Int, items : List Kindle.Item } -- SearchResponse
    -> JobStack
updateStack jobStack runningJobMaybe { totalPages, items } =
    case runningJobMaybe of
        Just (Search bn s page params) ->
            case nextPage totalPages page of
                Nothing ->
                    jobStack

                Just np ->
                    (Search bn s np params) :: jobStack

        Just (CollectPublishers browseNodes pubdate page foundPublishers) ->
            case List.map .publisher items of
                [] ->
                    nextBrowseNode browseNodes foundPublishers jobStack

                ps ->
                    let
                        newPublishers =
                            mergePublishers foundPublishers ps
                    in
                        nextPageOrDate totalPages browseNodes pubdate page foundPublishers newPublishers jobStack
                            |> L.info newPublishers

        Just (BrowseNodeLookup _) ->
            jobStack

        Just (ItemLookup _) ->
            jobStack

        Nothing ->
            jobStack


nextPage : Int -> Int -> Maybe Int
nextPage totalPages page =
    if totalPages <= 1 then
        Nothing
    else if page >= totalPages then
        Nothing
    else
        Just (page + 1)


nextBrowseNode : List Kindle.BrowseNode -> Set String -> JobStack -> JobStack
nextBrowseNode browseNodes foundPublishers jobStack =
    case browseNodes of
        [ _ ] ->
            -- Done collecting.
            L.info foundPublishers jobStack

        _ :: bns ->
            (CollectPublishers bns Seed.pubdateOrigin 1 foundPublishers) :: jobStack

        [] ->
            -- Should not happen; CollectPublishers job must not continue when all browseNodes are searched
            jobStack


mergePublishers : Set String -> List String -> Set String
mergePublishers foundPublishers =
    Set.fromList >> Set.union foundPublishers >> Set.remove Kindle.noPublisher


nextPageOrDate :
    Int
    -> List Kindle.BrowseNode
    -> Seed.Pubdate
    -> Int
    -> Set String
    -> Set String
    -> JobStack
    -> JobStack
nextPageOrDate totalPages browseNodes pubdate page foundPublishers newPublishers jobStack =
    case nextPage totalPages page of
        Nothing ->
            (CollectPublishers browseNodes (Seed.prevPubdate pubdate) 1 newPublishers) :: jobStack

        Just np ->
            if foundPublishers == newPublishers then
                (CollectPublishers browseNodes pubdate np newPublishers) :: jobStack
            else
                (CollectPublishers browseNodes pubdate 1 newPublishers) :: jobStack



-- Job Execution


exec : (Result PAAPI.Error Kindle.Response -> msg) -> PAAPI.Credentials -> PAAPI.AssociateTag -> Job -> Cmd msg
exec msg creds tag =
    task creds tag >> Task.attempt msg


task : PAAPI.Credentials -> PAAPI.AssociateTag -> Job -> Task PAAPI.Error Kindle.Response
task paapiCredentials associateTag job =
    case job of
        Search browseNode sort page params ->
            Kindle.search paapiCredentials associateTag browseNode sort page params

        BrowseNodeLookup browseNode ->
            Kindle.browseNodeLookup paapiCredentials associateTag browseNode

        ItemLookup asins ->
            Kindle.itemLookup paapiCredentials associateTag asins

        CollectPublishers browseNodes pubdate page foundPublishers ->
            case browseNodes of
                browseNode :: _ ->
                    Seed.power (Just pubdate) (Seed.Exclude foundPublishers)
                        |> Kindle.search paapiCredentials associateTag browseNode Kindle.DateRank page

                [] ->
                    -- Should not happen
                    Task.fail PAAPI.RateLimit
