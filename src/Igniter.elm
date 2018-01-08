module Igniter exposing (main)

{-| Amazon Product Advertising API (PAAPI) crawler.

This headless Elm program works as engine of the crawler.
Will be repeatedly called and collect information from PAAPI.

Note: Json.Decode is imported in order to hook inclusion on compile.
This should be unnecessary on Elm 0.19.

-}

import Platform
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
    { paapiCredentials = flags.paapiCredentials
    , associateTag = flags.associateTag
    , rateLimited = False
    , jobStack = Job.initStack flags.argv
    , runningJob = Nothing
    }
        |> logWithoutSensitive
        => []


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
                , jobStack = Job.updateStack model.jobStack model.runningJob res
            }
                |> L.dumpSearchResponse res
                => []

        PAAPIRes (Ok (Kindle.BrowseNodeLookup res)) ->
            { model | rateLimited = False, runningJob = Nothing }
                |> L.info res
                => []

        PAAPIRes (Ok (Kindle.ItemLookup res)) ->
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


onTick : Model -> Time -> ( Model, List (Cmd Msg) )
onTick ({ paapiCredentials, associateTag, jobStack, runningJob } as model) time =
    case ( jobStack, runningJob ) of
        ( j :: js, Nothing ) ->
            { model | jobStack = js, runningJob = Just j }
                => [ Job.exec PAAPIRes paapiCredentials associateTag j ]

        ( _, _ ) ->
            model => []


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
