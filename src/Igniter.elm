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
    PAAPI.Credentials


type Msg
    = IgniteMsg String
    | TickMsg Time
    | PAAPIRes (Result PAAPI.Error Kindle.SearchResult)


init : Flags -> ( Model, Cmd Msg )
init flags =
    { paapiCredentials = flags
    , previousResult = Nothing
    , running = False
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IgniteMsg text ->
            logAndThen text
                ( { model | running = True }, Cmd.none )

        TickMsg time ->
            logAndThen (Date.fromTime time)
                ( model, testPaapi model time )

        PAAPIRes (Ok res) ->
            logAndThen res
                ( { model | previousResult = Just res }, Cmd.none )

        PAAPIRes (Err PAAPI.Limit) ->
            logAndThen "Rate limited..."
                ( model, Cmd.none )

        PAAPIRes (Err unexpected) ->
            logAndThen unexpected
                ( model, Cmd.none )


logAndThen : log -> ret -> ret
logAndThen text ret =
    Debug.log "Info" text |> always ret


testPaapi : Model -> Time -> Cmd Msg
testPaapi { paapiCredentials, previousResult } time =
    let
        searchPage page =
            Kindle.search paapiCredentials PAAPIRes time Kindle.Root page [ "衿沢世衣子" ]
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
