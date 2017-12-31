module Igniter exposing (main)

{-| Amazon Product Advertising API (PAAPI) crawler.

This headless Elm program works as engine of the crawler.
Will be repeatedly called and collect information from PAAPI.

Note: Json.Decode is imported in order to hook inclusion on compile.
This should be unnecessary on Elm 0.19.

-}

import Platform
import Json.Decode exposing (..)
import Time exposing (Time)
import Task exposing (Task)
import Rocket exposing (..)
import Logger as L
import PAAPI
import PAAPI.Kindle as Kindle
import Igniter.Model exposing (Model)
import Igniter.Job as Job exposing (Job)


type alias Flags =
    { paapiCredentials : PAAPI.Credentials
    , associateTag : PAAPI.AssociateTag
    , argv : List String
    }


type Msg
    = TickMsg Time
    | PAAPIRes (Result PAAPI.Error Kindle.Response)


init : Flags -> ( Model, List (Cmd Msg) )
init flags =
    let
        options =
            Igniter.Model.parseOptions flags.argv
    in
        { paapiCredentials = flags.paapiCredentials
        , associateTag = flags.associateTag
        , options = options
        , rateLimited = False
        , jobStack = [ firstJob options ]
        , runningJob = Nothing
        }
            |> logWithoutSensitive
            => []


firstJob : Igniter.Model.Options -> Job
firstJob options =
    case options.mode of
        Igniter.Model.Search ->
            Job.Search Kindle.Boys Kindle.DateRank 1 "小学館" []

        Igniter.Model.BrowseNodeLookup ->
            Job.BrowseNodeLookup <| browseNode options.argv


browseNode : List String -> Kindle.BrowseNode
browseNode argv =
    case argv of
        [] ->
            Kindle.Root

        str :: _ ->
            Kindle.toBrowseNode str


logWithoutSensitive : Model -> Model
logWithoutSensitive model =
    { model | paapiCredentials = PAAPI.Credentials "XXX" "XXX" }
        |> Debug.log "Initial Model"
        |> always model



-- UPDATE


update : Msg -> Model -> ( Model, List (Cmd Msg) )
update msg model =
    case msg of
        TickMsg time ->
            L.info time <|
                onTick model time

        PAAPIRes (Ok (Kindle.Search res)) ->
            { model
                | rateLimited = False
                , runningJob = Nothing
                , jobStack = scheduleNextSearch model res
            }
                |> L.dumpSearchResponse res
                => []

        PAAPIRes (Ok (Kindle.BrowseNodeLookup res)) ->
            { model | rateLimited = False, runningJob = Nothing }
                |> L.info res
                => []

        PAAPIRes (Err PAAPI.RateLimit) ->
            { model | rateLimited = True }
                |> repush
                |> L.rateLimit model.rateLimited
                => []

        PAAPIRes (Err (PAAPI.HttpError httpError)) ->
            { model | runningJob = Nothing }
                |> L.httpError False httpError
                => []


repush : Model -> Model
repush ({ jobStack, runningJob } as model) =
    let
        newStack =
            case runningJob of
                Just j ->
                    j :: jobStack

                Nothing ->
                    jobStack
    in
        { model | jobStack = newStack, runningJob = Nothing }


onTick : Model -> Time -> ( Model, List (Cmd Msg) )
onTick ({ paapiCredentials, associateTag, jobStack, runningJob } as model) time =
    case ( jobStack, runningJob ) of
        ( j :: js, Nothing ) ->
            { model | jobStack = js, runningJob = Just j }
                => [ Job.task paapiCredentials associateTag j |> Task.attempt PAAPIRes ]

        ( _, _ ) ->
            model => []


scheduleNextSearch : Model -> { x | totalPages : Int } -> Job.JobStack
scheduleNextSearch { jobStack, runningJob } { totalPages } =
    case Maybe.andThen (Job.nextPage totalPages) runningJob of
        Nothing ->
            jobStack

        Just j ->
            j :: jobStack



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    []
        |> listenTick model
        |> Sub.batch


listenTick : Model -> List (Sub Msg) -> List (Sub Msg)
listenTick { jobStack, runningJob, rateLimited } subs =
    if List.isEmpty jobStack && runningJob == Nothing then
        subs
    else
        Time.every (interval rateLimited) TickMsg :: subs


interval : Bool -> Time
interval rateLimited =
    if rateLimited then
        1000 * Time.millisecond
    else
        200 * Time.millisecond



-- MAIN


main : Platform.Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init >> batchInit
        , update = update >> batchUpdate
        , subscriptions = subscriptions
        }
