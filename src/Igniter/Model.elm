module Igniter.Model exposing (Model)

import PAAPI
import Igniter.Job as Job exposing (Job, JobStack)


type alias Model =
    { paapiCredentials : PAAPI.Credentials
    , associateTag : PAAPI.AssociateTag
    , rateLimited : Bool
    , jobStack : JobStack
    , runningJob : Maybe Job
    }
