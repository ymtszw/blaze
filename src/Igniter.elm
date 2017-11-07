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
import Date
import XmlParser
import PAAPI
import Xml
import Igniter.Model exposing (Model)
import Igniter.Fuse as Fuse
import Igniter.Kindle as Kindle


type alias Flags =
    { paapiCredentials : PAAPI.Credentials
    , argv : List String
    }


type Msg
    = IgniteMsg String
    | TickMsg Time
    | SearchRes (Result PAAPI.Error Kindle.SearchResult)
    | BrowseNodeRes (Result PAAPI.Error Kindle.BrowseNodeLookupResult)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { paapiCredentials = flags.paapiCredentials
    , options = Igniter.Model.parseOptions flags.argv
    , previousResult = Nothing
    , running = False
    }
        |> Debug.log "Initial Model"
        |> flip (!) []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IgniteMsg text ->
            logAndThen text
                ( { model | running = True }, Cmd.none )

        TickMsg time ->
            logAndThen (Date.fromTime time)
                ( model, onTick model time )

        SearchRes (Ok res) ->
            logAndThen res
                ( { model | previousResult = Just res }, Cmd.none )

        SearchRes (Err PAAPI.Limit) ->
            logAndThen "Rate limited..."
                ( model, Cmd.none )

        SearchRes (Err unexpected) ->
            logAndThen unexpected
                ( model, Cmd.none )

        BrowseNodeRes (Ok res) ->
            logAndThen res
                ( { model | running = False }, Cmd.none )

        BrowseNodeRes (Err PAAPI.Limit) ->
            logAndThen "Rate limited..."
                ( model, Cmd.none )

        BrowseNodeRes (Err unexpected) ->
            logAndThen unexpected
                ( model, Cmd.none )


logAndThen : log -> ret -> ret
logAndThen text ret =
    Debug.log "Info" text |> always ret


onTick : Model -> Time -> Cmd Msg
onTick ({ paapiCredentials, options } as model) time =
    case options.mode of
        Igniter.Model.Search ->
            testPaapi model time

        Igniter.Model.LookupBrowseNode ->
            let
                browseNode =
                    case options.argv of
                        [] ->
                            Kindle.Root

                        str :: _ ->
                            Kindle.toBrowseNode str
            in
                Kindle.lookupBrowseNode paapiCredentials BrowseNodeRes time browseNode


testPaapi : Model -> Time -> Cmd Msg
testPaapi { paapiCredentials, previousResult } time =
    let
        searchPage page =
            Kindle.search paapiCredentials SearchRes time Kindle.Root page [ "衿沢世衣子" ]
    in
        case previousResult of
            Nothing ->
                searchPage 1

            Just { totalPages, currentPage } ->
                if totalPages == currentPage || currentPage == 10 then
                    Cmd.none
                else
                    searchPage (currentPage + 1)


subscriptions : Model -> Sub Msg
subscriptions ({ running } as model) =
    Fuse.ignite IgniteMsg
        :: (if running then
                [ Time.every (1 * Time.second) TickMsg ]
            else
                []
           )
        |> Sub.batch


main : Platform.Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
