module PAAPI.Kindle
    exposing
        ( BrowseNode(..)
        , Sort(..)
        , Item
        , Response(..)
        , toBrowseNode
        , search
        , browseNodeLookup
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

<http://docs.aws.amazon.com/AWSECommerceService/latest/DG/ItemSearch_PowerSearches.html>

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


type Response
    = Search
        { totalPages : Int
        , currentPage : Int
        , items : List Item
        }
    | BrowseNodeLookup
        { id : String
        , name : String
        , children : List AdjacentBrowseNode
        , ancestors : List AdjacentBrowseNode
        }


type alias AdjacentBrowseNode =
    { id : String, name : String }


searchResponse : Int -> Int -> List Item -> Response
searchResponse tp cp is =
    Search
        { totalPages = tp
        , currentPage = cp
        , items = is
        }


browseNodeLookupResponse : String -> String -> List AdjacentBrowseNode -> List AdjacentBrowseNode -> Response
browseNodeLookupResponse i n c a =
    BrowseNodeLookup
        { id = i
        , name = n
        , children = c
        , ancestors = a
        }


{-| Searches items with given parameters.
-}
search : PAAPI.Credentials -> (Result PAAPI.Error Response -> msg) -> Time -> BrowseNode -> Sort -> Int -> List String -> Cmd msg
search creds msg time browseNode sort_ page keywords =
    PAAPI.get creds
        searchResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = searchParams browseNode sort_ page keywords
        }


searchResultDecoder : XD.Decoder Response
searchResultDecoder =
    XD.path [ "Items" ] <|
        XD.singleton <|
            (XD.succeed searchResponse
                |> XDP.requiredPath [ "TotalPages" ] (XD.singleton XD.int)
                |> XDP.optionalPath [ "Request", "ItemSearchRequest", "ItemPage" ] (XD.singleton XD.int) 1
                |> XDP.requiredPath [ "Item" ] (XD.list itemDecoder)
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
    , "Power" => power keywords
    ]


power : List String -> String
power keywords =
    [ "not 分冊"
    , "not 雑誌"
    , "not 画集"
    , "pubdate: during 11-2017"
    ]
        |> (++) keywords
        |> String.join " and "


{-| Retrieves existing BrowseNodes relative to the given `BrowseNode`.
-}
browseNodeLookup : PAAPI.Credentials -> (Result PAAPI.Error Response -> msg) -> Time -> BrowseNode -> Cmd msg
browseNodeLookup creds msg time browseNode =
    PAAPI.get creds
        browseNodeLookupResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = browseNodeParams browseNode
        }


browseNodeLookupResultDecoder : XD.Decoder Response
browseNodeLookupResultDecoder =
    XD.path [ "BrowseNodes", "BrowseNode" ] <|
        XD.singleton <|
            (XD.succeed browseNodeLookupResponse
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
