port module Worker exposing (main)

import Platform
import Json.Decode
import Time exposing (Time)


type alias Model =
    { running : Bool
    , terminating : Bool
    }


type Msg
    = StartMsg String
    | InterruptMsg ()
    | TickMsg Time


init : ( Model, Cmd Msg )
init =
    ( { running = False, terminating = False }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartMsg text ->
            text
                |> Debug.log "Started"
                |> always ( { model | running = not model.terminating }, Cmd.none )

        TickMsg time ->
            time
                |> Debug.log "Tick"
                |> always ( model, Cmd.none )

        InterruptMsg () ->
            Debug.log "Interrupted" "Terminating now!"
                |> always ( { model | running = False, terminating = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions { running } =
    Sub.batch
        [ interrupt InterruptMsg
        , (if running then
            Time.every (5 * Time.second) TickMsg
           else
            start StartMsg
          )
        ]


port start : (String -> msg) -> Sub msg


port interrupt : (() -> msg) -> Sub msg


main : Platform.Program Never Model Msg
main =
    Platform.program
        { init = init
        , update = update
        , subscriptions = subscriptions
        }
