module Igniter exposing (..)

{-| Amazon Product Advertising API (PAAPI) crawler.

This headless Elm program works as engine of the crawler.
Will be repeatedly called and collect information from PAAPI.

Note: Json.Decode is imported in order to hook inclusion on compile.
This should be unnecessary on Elm 0.19.

-}

import Platform
import Json.Decode exposing (..)
import Time exposing (Time)
import Logger as L
import PAAPI
import PAAPI.Kindle as Kindle
import Igniter.Model exposing (Model)
import Igniter.Job as Job exposing (Job)


type alias Flags =
    { paapiCredentials : PAAPI.Credentials
    , argv : List String
    }


type Msg
    = TickMsg Time
    | PAAPIRes (Result PAAPI.Error Kindle.Response)


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        options =
            Igniter.Model.parseOptions flags.argv
    in
        { paapiCredentials = flags.paapiCredentials
        , options = options
        , rateLimited = False
        , jobStack = [ firstJob options ]
        , runningJob = Nothing
        }
            |> logWithoutSensitive
            |> flip (!) []


firstJob : Igniter.Model.Options -> Job
firstJob options =
    case options.mode of
        Igniter.Model.Search ->
            Job.Search Kindle.Boys Kindle.DateRank 1 "講談社" []

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
    { model | paapiCredentials = PAAPI.Credentials "XXX" "XXX" "XXX" }
        |> Debug.log "Initial Model"
        |> always model



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TickMsg time ->
            L.info time <|
                onTick model time

        PAAPIRes (Ok (Kindle.Search res)) ->
            L.dumpSearchResponse res
                ( { model
                    | rateLimited = False
                    , runningJob = Nothing
                    , jobStack = scheduleNextSearch model res
                  }
                , Cmd.none
                )

        PAAPIRes (Ok (Kindle.BrowseNodeLookup res)) ->
            L.info res
                ( { model | rateLimited = False, runningJob = Nothing }, Cmd.none )

        PAAPIRes (Err PAAPI.Limit) ->
            L.rateLimit model.rateLimited
                ( repush { model | rateLimited = True }, Cmd.none )

        PAAPIRes (Err (PAAPI.Fail httpError)) ->
            L.httpError False
                httpError
                ( { model | runningJob = Nothing }, Cmd.none )


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


onTick : Model -> Time -> ( Model, Cmd Msg )
onTick ({ jobStack, runningJob } as model) time =
    case ( jobStack, runningJob ) of
        ( j :: js, Nothing ) ->
            ( { model | jobStack = js, runningJob = Just j }
            , runJob model time j
            )

        ( _, _ ) ->
            ( model, Cmd.none )


runJob : Model -> Time -> Job -> Cmd Msg
runJob { paapiCredentials } time job =
    case job of
        Job.Search browseNode sort page publisher keywords ->
            Kindle.search paapiCredentials PAAPIRes time browseNode sort page publisher keywords

        Job.BrowseNodeLookup browseNode ->
            Kindle.browseNodeLookup paapiCredentials PAAPIRes time browseNode


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
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
