module Igniter exposing (..)

{-| Amazon Product Advertising API (PAAPI) crawler.

This headless Elm program works as engine of the crawler.
Will be repeatedly called and correct information from PAAPI.

Note: Json.Decode is imported in order to hook inclusion on compile.
This should be unnecessary on Elm 0.19.

-}

import Platform
import Json.Decode exposing (..)
import Igniter.Model exposing (Model, PAAPICredentials)
import Igniter.Fuse as IF


type alias Flags =
    PAAPICredentials


type Msg
    = ModelDumpMsg ()


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { paapiCredentials = flags
      , someFlag = True
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ModelDumpMsg () ->
            ( model, IF.sendModelDump model )


subscriptions : Model -> Sub Msg
subscriptions model =
    IF.requestModelDump ModelDumpMsg


main : Platform.Program Flags Model Msg
main =
    Platform.programWithFlags
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
