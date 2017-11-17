module Logger exposing (info, dumpSearchResponse, rateLimit, httpError)

import Date
import Http
import Util
import PAAPI.Kindle as Kindle


info : a -> b -> b
info text ret =
    Debug.log "Info" text |> always ret


dumpSearchResponse : { x | totalPages : Int, currentPage : Int, items : List Kindle.Item } -> b -> b
dumpSearchResponse { totalPages, currentPage, items } ret =
    items
        |> List.map logItem
        |> always (Debug.log "Search Result" (toString currentPage ++ "/" ++ toString totalPages))
        |> always ret


logItem : { x | asin : String, title : String, authors : List String, releaseDate : Date.Date } -> String
logItem { asin, title, authors, releaseDate } =
    Debug.log asin <| title ++ " [" ++ String.join ", " authors ++ "] (@" ++ Util.ymd releaseDate ++ ")"


rateLimit : Bool -> a -> a
rateLimit alreadyLimited ret =
    if alreadyLimited then
        ret
    else
        info "Rate limited..." ret


httpError : Bool -> Http.Error -> a -> a
httpError verbose httpError_ ret =
    -- verbose is a compile time parameter
    case httpError_ of
        Http.BadUrl url ->
            Debug.log "Malformed URL" url |> always ret

        Http.Timeout ->
            Debug.log "Error" "Timeout" |> always ret

        Http.NetworkError ->
            Debug.log "Error" "Server unreachable" |> always ret

        Http.BadStatus { url, status, body } ->
            Debug.log "Body" body
                |> always (Debug.log (status.message ++ " " ++ toString status.code) url)
                |> always ret

        Http.BadPayload message { body } ->
            if verbose then
                Debug.log "Raw Body" body
                    |> always (Debug.log "Invalid Body" message)
                    |> always ret
            else
                Debug.log "Invalid Body" message
                    |> always ret
