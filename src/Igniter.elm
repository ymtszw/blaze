module Igniter exposing (..)

{-| Amazon Product Advertising API (PAAPI) crawler.

This headless Elm program works as engine of the crawler.
Will be repeatedly called and correct information from PAAPI.

Note: Json.Decode is imported in order to hook inclusion on compile.
This should be unnecessary on Elm 0.19.

-}

import Platform
import Json.Decode exposing (..)
import Time exposing (Time)
import Date
import Http
import Util exposing ((=>))
import PAAPI
import Igniter.Model exposing (Model)
import Igniter.Fuse as Fuse
import Igniter.Kindle as Kindle


type alias Flags =
    PAAPI.Credentials


type Msg
    = IgniteMsg String
    | TickMsg Time
    | PAAPIRes (Result Http.Error String)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { paapiCredentials = flags
      , running = False
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        IgniteMsg text ->
            text
                |> Debug.log "Igniter started"
                |> always ( { model | running = True }, Cmd.none )

        TickMsg time ->
            time
                |> Date.fromTime
                |> Debug.log "Tick"
                |> always ( model, testPaapi model time )

        PAAPIRes result ->
            result
                |> Debug.log "Response"
                |> always ( model, Cmd.none )


testPaapi : Model -> Time -> Cmd Msg
testPaapi { paapiCredentials } time =
    Kindle.search paapiCredentials PAAPIRes time Kindle.Root [ "衿沢世衣子" ]


subscriptions : Model -> Sub Msg
subscriptions ({ running } as model) =
    Fuse.ignite IgniteMsg
        :: (if running then
                [ Time.every (5 * Time.second) TickMsg ]
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
