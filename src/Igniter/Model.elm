module Igniter.Model exposing (Model, PAAPICredentials)


type alias PAAPICredentials =
    { accessKeyId : String
    , secretAccessKey : String
    , associateTag : String
    }


type alias Model =
    { paapiCredentials : PAAPICredentials
    , running : Bool
    }
