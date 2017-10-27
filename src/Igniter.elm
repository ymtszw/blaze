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
    | PAAPIRes (Result PAAPI.Error XmlParser.Xml)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { paapiCredentials = flags, running = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IgniteMsg text ->
            logAndThen text
                ( { model | running = True }, Cmd.none )

        TickMsg time ->
            logAndThen (Date.fromTime time)
                ( model, testPaapi model time )

        PAAPIRes (Ok xml) ->
            ( model, Cmd.none )
                |> dumpTestRes xml

        PAAPIRes (Err PAAPI.Limit) ->
            logAndThen "Rate limited..."
                ( model, Cmd.none )

        PAAPIRes (Err unexpected) ->
            logAndThen unexpected
                ( model, Cmd.none )


dumpTestRes : XmlParser.Xml -> ret -> ret
dumpTestRes xml ret =
    xml
        |> Xml.q [ "Items", "Item", "ItemAttributes", "Title", "$text" ]
        |> logAndThen "Fetched titles"
        |> List.foldl (\t i -> t |> Xml.text |> Debug.log (i |> toString |> String.padLeft 4 ' ') |> always (i + 1)) 1
        |> always ret


logAndThen : log -> ret -> ret
logAndThen text ret =
    Debug.log "Info" text |> always ret


testPaapi : Model -> Time -> Cmd Msg
testPaapi { paapiCredentials } time =
    Kindle.search paapiCredentials PAAPIRes time Kindle.Root [ "衿沢世衣子" ]


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
