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
import Igniter.Model exposing (Model, PAAPICredentials)
import Igniter.Fuse as IF


type alias Flags =
    PAAPICredentials


type Msg
    = IgniteMsg String
    | TickMsg Time


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
                |> always ( model, IF.sendModelDump model )


subscriptions : Model -> Sub Msg
subscriptions ({ running } as model) =
    IF.ignite IgniteMsg
        :: (if running then
                [ Time.every (10 * Time.second) TickMsg ]
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
