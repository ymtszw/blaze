module Igniter.Model exposing (Model)

import PAAPI


type alias Model =
    { paapiCredentials : PAAPI.Credentials
    , running : Bool
    }
