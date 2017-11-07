module Igniter.Kindle
    exposing
        ( BrowseNode(..)
        , Item
        , SearchResult
        , BrowseNodeLookupResult
        , toBrowseNode
        , search
        , lookupBrowseNode
        )

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


{-| Possible BrowseNodes in JP domain.

It may be better to store them in database for runtime update,
though at this time I just hard-code them.

You can acquire latest BrowseNodes by:

    $ node dist/igniter.js BrowseNode <BrowseNode(Id)>

Uninterested BrowseNodes can be given in raw `BrowseNodeId String` form.

-}
type BrowseNode
    = Root
    | Book
    | Comic
    | BrowseNodeId String


toBrowseNode : String -> BrowseNode
toBrowseNode str =
    case str of
        "Book" ->
            Book

        "Comic" ->
            Comic

        _ ->
            case String.toInt str of
                Ok _ ->
                    BrowseNodeId str

                Err _ ->
                    Root


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


type alias BrowseNodeLookupResult =
    { id : String
    , name : String
    , children : List BrowseNodeChild
    }


type alias BrowseNodeChild =
    { id : String, name : String }


{-| Searches items with given parameters.
-}
search : PAAPI.Credentials -> (Result PAAPI.Error SearchResult -> msg) -> Time -> BrowseNode -> Int -> List String -> Cmd msg
search creds msg time browseNode page keywords =
    PAAPI.get creds
        searchResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = searchParams browseNode page keywords
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


searchParams : BrowseNode -> Int -> List String -> KVS
searchParams browseNode page keywords =
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

        Book ->
            "2275256051"

        Comic ->
            "2293143051"

        BrowseNodeId numStr ->
            numStr


setKeywords : List String -> KVS -> KVS
setKeywords keywords params =
    case keywords of
        [] ->
            params

        _ ->
            ( "Keywords", String.join "," keywords ) :: params


{-| Retrieves existing BrowseNodes relative to the given `BrowseNode`.
-}
lookupBrowseNode : PAAPI.Credentials -> (Result PAAPI.Error BrowseNodeLookupResult -> msg) -> Time -> BrowseNode -> Cmd msg
lookupBrowseNode creds msg time browseNode =
    PAAPI.get creds
        browseNodeLookupResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = browseNodeParams browseNode
        }


browseNodeLookupResultDecoder : XD.Decoder BrowseNodeLookupResult
browseNodeLookupResultDecoder =
    XD.succeed BrowseNodeLookupResult
        |> XDP.requiredPath [ "BrowseNodes", "BrowseNode", "BrowseNodeId" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "BrowseNodes", "BrowseNode", "Name" ] (XD.singleton XD.string)
        |> XDP.optionalPathWithDefault [ "BrowseNodes", "BrowseNode", "Children", "BrowseNode" ] (XD.list browseNodeChildDecoder) []


browseNodeChildDecoder : XD.Decoder BrowseNodeChild
browseNodeChildDecoder =
    XD.succeed BrowseNodeChild
        |> XDP.requiredPath [ "BrowseNodeId" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "Name" ] (XD.singleton XD.string)


browseNodeParams : BrowseNode -> KVS
browseNodeParams browseNode =
    [ "BrowseNodeId" => browseNodeId browseNode
    , "Operation" => "BrowseNodeLookup"
    ]
