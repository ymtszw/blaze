module Igniter.Kindle exposing (BrowseNode(..), Item, SearchResult, search)

{-| Wrapper client module for PAAPI that queries for items in Kindle store.

Reference of available search parameters for JP Kindle store:

<pre>
| SearchIndex | Root Browse Node | Sort Values          | ItemSearch Parameters |
|:----------- |:---------------- |:-------------------- |:--------------------- |
| KindleStore | 2250739051       | relevancerank        | Author                |
|             |                  | salesrank            | Availability          |
|             |                  | price                | ItemPage              |
|             |                  | -price               | Keywords              |
|             |                  | reviewrank           | MaximumPrice          |
|             |                  | reviewrank_authority | MerchantId            |
|             |                  | daterank             | MinPercentageOff      |
|             |                  |                      | MinimumPrice          |
|             |                  |                      | Publisher             |
|             |                  |                      | Sort                  |
|             |                  |                      | Title                 |
</pre>

<http://docs.aws.amazon.com/AWSECommerceService/latest/DG/LocaleJP.html>

-}

import Time exposing (Time)
import XmlParser exposing (Xml, Node(..))
import Util exposing (KVS, (=>))
import Xml.Decode as XD
import Xml.Decode.Pipeline as XDP
import PAAPI


type BrowseNode
    = Root


type alias Item =
    { asin : String
    , title : String
    , authors : List String
    , manufacturer : Maybe String
    }


type alias SearchResult =
    { totalPages : Int
    , currentPage : Int
    , items : List Item
    }


search : PAAPI.Credentials -> (Result PAAPI.Error SearchResult -> msg) -> Time -> BrowseNode -> Int -> List String -> Cmd msg
search creds msg time browseNode page keywords =
    PAAPI.get creds
        searchResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = params browseNode page keywords
        }


searchResultDecoder : XD.Decoder SearchResult
searchResultDecoder =
    XD.succeed SearchResult
        |> XDP.requiredPath [ "Items", "TotalPages" ] (XD.singleton XD.int)
        |> XDP.optionalPathWithDefault [ "Items", "Request", "ItemSearchRequest", "ItemPage" ] (XD.singleton XD.int) 1
        |> XDP.requiredPath [ "Items", "Item" ] (XD.list itemDecoder)


itemDecoder : XD.Decoder Item
itemDecoder =
    XD.succeed Item
        |> XDP.requiredPath [ "ASIN" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "Title" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "Author" ] (XD.list XD.string)
        |> XDP.optionalPath [ "ItemAttributes", "Manufacturer" ] (XD.singleton XD.string)


params : BrowseNode -> Int -> List String -> KVS
params browseNode page keywords =
    setKeywords keywords
        [ "SearchIndex" => searchIndex
        , "Operation" => "ItemSearch"
        , "BrowseNode" => browseNodeId browseNode
        , "Sort" => "daterank"
        , "ItemPage" => toString page
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
