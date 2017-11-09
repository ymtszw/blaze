module PAAPI.Kindle
    exposing
        ( BrowseNode(..)
        , Sort(..)
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

Although, in order to use Power Search feature, we need to use SearchIndex: Books.
KindleStore

<pre>
| SearchIndex | Root Browse Node | Sort Values          | ItemSearch Parameters |
|:----------- |:---------------- |:-------------------- |:--------------------- |
| Books       | 465610           | salesrank            | Author                |
|             |                  | pricerank            | Availability          |
|             |                  | inverse-pricerank    | ItemPage              |
|             |                  | daterank             | Keywords              |
|             |                  | titlerank            | MaximumPrice          |
|             |                  | -titlerank           | MerchantId            |
|             |                  | price                | MinPercentageOff      |
|             |                  | -price               | MinimumPrice          |
|             |                  | -publication_date    | Power                 |
|             |                  | -unit-sales          | Publisher             |
|             |                  |                      | Sort                  |
|             |                  |                      | Title                 |
</pre>

<http://docs.aws.amazon.com/AWSECommerceService/latest/DG/LocaleJP.html>

-}

import Date exposing (Date)
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


{-| Possible sort orders for JP Kindle store.

All "XXXRank" sorts are ascending.

-}
type Sort
    = RelevanceRank
    | SalesRank
    | PriceAsc
    | PriceDsc
    | ReviewRank
    | ReviewRankAuthority
    | DateRank


sort : Sort -> String
sort sort_ =
    case sort_ of
        PriceAsc ->
            "price"

        PriceDsc ->
            "-price"

        other ->
            other |> toString |> String.toLower


type alias Item =
    { asin : String
    , title : String
    , releaseDate : Date
    , authors : List String
    , publisher : String -- This may not present for self-published/dojin items
    }


type alias SearchResult =
    { totalPages : Int
    , currentPage : Int
    , items : List Item
    }


type alias BrowseNodeLookupResult =
    { id : String
    , name : String
    , children : List AdjacentBrowseNode
    , ancestors : List AdjacentBrowseNode
    }


type alias AdjacentBrowseNode =
    { id : String, name : String }


{-| Searches items with given parameters.
-}
search : PAAPI.Credentials -> (Result PAAPI.Error SearchResult -> msg) -> Time -> BrowseNode -> Sort -> Int -> List String -> Cmd msg
search creds msg time browseNode sort_ page keywords =
    PAAPI.get creds
        searchResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = searchParams browseNode sort_ page keywords
        }


searchResultDecoder : XD.Decoder SearchResult
searchResultDecoder =
    XD.path [ "Items" ] <|
        XD.singleton <|
            (XD.succeed SearchResult
                |> XDP.requiredPath [ "TotalPages" ] (XD.singleton XD.int)
                |> XDP.optionalPath [ "Request", "ItemSearchRequest", "ItemPage" ] (XD.singleton XD.int) 1
                |> XDP.requiredPath [ "Item" ] (XD.leakyList itemDecoder)
             -- Ignores Items without essential properties
            )


itemDecoder : XD.Decoder Item
itemDecoder =
    XD.succeed Item
        |> XDP.requiredPath [ "ASIN" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "Title" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "ReleaseDate" ] (XD.singleton XD.date)
        |> XDP.requiredPath [ "ItemAttributes", "Author" ] (XD.list XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "Publisher" ] (XD.singleton XD.string)


searchParams : BrowseNode -> Sort -> Int -> List String -> KVS
searchParams browseNode sort_ page keywords =
    [ "Operation" => "ItemSearch"
    , "SearchIndex" => "Books" -- Required for Power Search
    , "ResponseGroup" => "Request,ItemAttributes"
    , "BrowseNode" => browseNodeId browseNode
    , "Sort" => sort sort_
    , "ItemPage" => toString page
    , "Power" => power
    ]


power : String
power =
    [ "not title: 分冊"
    , "not title: 雑誌"
    , "pubdate: during 11-2017"
    , "publisher: 講談社"
    ]
        |> String.join " and "


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
    XD.path [ "BrowseNodes", "BrowseNode" ] <|
        XD.singleton <|
            (XD.succeed BrowseNodeLookupResult
                |> XDP.requiredPath [ "BrowseNodeId" ] (XD.singleton XD.string)
                |> XDP.requiredPath [ "Name" ] (XD.singleton XD.string)
                |> XDP.optionalPath [ "Children", "BrowseNode" ] (XD.list adjacentBrowseNodeDecoder) []
                |> XDP.optionalPath [ "Ancestors", "BrowseNode" ] (XD.list adjacentBrowseNodeDecoder) []
            )


adjacentBrowseNodeDecoder : XD.Decoder AdjacentBrowseNode
adjacentBrowseNodeDecoder =
    XD.succeed AdjacentBrowseNode
        |> XDP.requiredPath [ "BrowseNodeId" ] (XD.singleton XD.string)
        |> XDP.requiredPath [ "Name" ] (XD.singleton XD.string)


browseNodeParams : BrowseNode -> KVS
browseNodeParams browseNode =
    [ "BrowseNodeId" => browseNodeId browseNode
    , "Operation" => "BrowseNodeLookup"
    ]
