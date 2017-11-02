module Igniter.Model exposing (Model)

import PAAPI
import Igniter.Kindle


type alias Model =
    { paapiCredentials : PAAPI.Credentials
    , previousResult : Maybe Igniter.Kindle.SearchResult
    , running : Bool
    }
