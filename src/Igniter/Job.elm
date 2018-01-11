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
    | CollectPublishers Seed.Pubdate (List Kindle.BrowseNode) Seed.Pubdate Int (Set String)
    | RankPublishers (List String) (List ( String, Int ))


type alias JobStack =
    List Job



-- Job Initizlization


initStack : List String -> List String -> JobStack
initStack knownPublishers argv =
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
                    [ CollectPublishers (Seed.pubdateFromArgs tail) Seed.comicNodes Seed.pubdateOrigin 1 Seed.wellknownPublishers ]

                "rankpublishers" ->
                    [ RankPublishers knownPublishers [] ]

                _ ->
                    [ Search Kindle.Comic Kindle.DateRank 1 tail ]


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
    -> Kindle.SearchResponse
    -> JobStack
updateStack jobStack runningJobMaybe { totalPages, items } =
    case runningJobMaybe of
        Just (Search bn s page params) ->
            case nextPage totalPages page of
                Nothing ->
                    jobStack

                Just np ->
                    (Search bn s np params) :: jobStack

        Just (CollectPublishers pubdateLimit browseNodes pubdate page foundPublishers) ->
            case List.map .publisher items of
                [] ->
                    if pubdate == pubdateLimit then
                        nextBrowseNode pubdateLimit browseNodes foundPublishers jobStack
                    else
                        (CollectPublishers pubdateLimit browseNodes (Seed.nextPubdate pubdate) 1 foundPublishers) :: jobStack

                ps ->
                    let
                        newPublishers =
                            mergePublishers foundPublishers ps
                                |> Debug.log "Found Publishers"
                    in
                        if foundPublishers == newPublishers then
                            case nextPage totalPages page of
                                Nothing ->
                                    if pubdate == pubdateLimit then
                                        nextBrowseNode pubdateLimit browseNodes foundPublishers jobStack
                                    else
                                        (CollectPublishers pubdateLimit browseNodes (Seed.nextPubdate pubdate) 1 foundPublishers) :: jobStack

                                Just np ->
                                    (CollectPublishers pubdateLimit browseNodes pubdate np foundPublishers) :: jobStack
                        else
                            (CollectPublishers pubdateLimit browseNodes pubdate 1 newPublishers) :: jobStack

        Just (BrowseNodeLookup _) ->
            jobStack

        Just (ItemLookup _) ->
            jobStack

        Just (RankPublishers (kp :: kps) publisherIndex) ->
            (RankPublishers kps (( kp, totalPages ) :: publisherIndex)) :: jobStack

        Just (RankPublishers [] publisherIndex) ->
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


nextBrowseNode : Seed.Pubdate -> List Kindle.BrowseNode -> Set String -> JobStack -> JobStack
nextBrowseNode pubdateLimit browseNodes foundPublishers jobStack =
    case browseNodes of
        _ :: bns ->
            (CollectPublishers pubdateLimit bns Seed.pubdateOrigin 1 foundPublishers) :: jobStack

        [] ->
            L.info foundPublishers jobStack


mergePublishers : Set String -> List String -> Set String
mergePublishers foundPublishers =
    Set.fromList >> Set.union foundPublishers >> Set.remove Kindle.noPublisher



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

        CollectPublishers _ (browseNode :: _) pubdate page foundPublishers ->
            Seed.power (Just pubdate) (Seed.Exclude foundPublishers)
                |> Kindle.search paapiCredentials associateTag browseNode Kindle.DateRank page

        RankPublishers (kp :: _) publisherIndex ->
            Seed.power Nothing (Seed.By kp)
                |> Kindle.search paapiCredentials associateTag Kindle.Comic Kindle.DateRank 1

        _ ->
            Task.succeed Kindle.Terminate
