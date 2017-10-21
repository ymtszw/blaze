module Igniter.Kindle exposing (BrowseNode(..), search)

{-| Wrapper client module for PAAPI that queries for items in Kindle store.

Reference of available search parameters for JP Kindle store:

| SearchIndex | Root Browse Node | Sort Values | ItemSearch Parameters |
|:--- |:--- |:--- |:--- |
| KindleStore | 2250739051 | relevancerank | Author |
| | | salesrank | Availability |
| | | price | ItemPage |
| | | -price | Keywords |
| | | reviewrank | MaximumPrice |
| | | reviewrank_authority | MerchantId |
| | | daterank | MinPercentageOff |
| | | | MinimumPrice |
| | | | Publisher |
| | | | Sort |
| | | | Title |

<http://docs.aws.amazon.com/AWSECommerceService/latest/DG/LocaleJP.html>

-}

import Time exposing (Time)
import Http
import Util exposing (KVS, (=>))
import PAAPI


type BrowseNode
    = Root


search : PAAPI.Credentials -> (Result Http.Error String -> msg) -> Time -> BrowseNode -> List String -> Cmd msg
search creds msg time browseNode keywords =
    PAAPI.get creds
        msg
        time
        { locale = PAAPI.JP
        , params = params browseNode keywords
        }


params : BrowseNode -> List String -> KVS
params browseNode keywords =
    setKeywords keywords
        [ "SearchIndex" => searchIndex
        , "Operation" => "ItemSearch"
        , "BrowseNode" => browseNodeId browseNode
        , "Sort" => "daterank"
        ]


searchIndex : String
searchIndex =
    "KindleStore"


browseNodeId : BrowseNode -> String
browseNodeId browseNode =
    case browseNode of
        Root ->
            "2250739051"


setKeywords : List String -> KVS -> KVS
setKeywords keywords params =
    case keywords of
        [] ->
            params

        _ ->
            ( "Keywords", String.join "," keywords ) :: params
