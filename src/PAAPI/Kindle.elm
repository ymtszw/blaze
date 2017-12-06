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
    | Boys
    | Mens
    | Girls
    | Ladies
    | BrowseNodeId String


toBrowseNode : String -> BrowseNode
toBrowseNode str =
    case str of
        "Book" ->
            Book

        "Comic" ->
            Comic

        "Boys" ->
            Boys

        "Mens" ->
            Mens

        "Girls" ->
            Girls

        "Ladies" ->
            Ladies

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

        Boys ->
            "2430812051"

        Mens ->
            "2430869051"

        Girls ->
            "2430765051"

        Ladies ->
            "2430737051"

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


type alias Item =
    { asin : String
    , title : String
    , releaseDate : Date
    , authors : List String
    , publisher : String -- This may not present for self-published/dojin items
    , links : Links
    }


type alias Links =
    { detailPageUrl : String
    , smallImageUrl : String
    , mediumImageUrl : String
    , largeImageUrl : String
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
search : PAAPI.Credentials -> (Result PAAPI.Error Response -> msg) -> Time -> BrowseNode -> Sort -> Int -> String -> List String -> Cmd msg
search creds msg time browseNode sort_ page publisher keywords =
    PAAPI.get creds
        searchResultDecoder
        msg
        time
        { locale = PAAPI.JP
        , params = searchParams browseNode sort_ page publisher keywords
        }


searchResultDecoder : XD.Decoder Response
searchResultDecoder =
    XD.path [ "Items" ] <|
        XD.single <|
            (XD.succeed searchResponse
                |> XDP.requiredPath [ "TotalPages" ] (XD.single XD.int)
                |> XDP.optionalPath [ "Request", "ItemSearchRequest", "ItemPage" ] (XD.single XD.int) 1
                |> XDP.requiredPath [ "Item" ] (XD.list itemDecoder)
            )


itemDecoder : XD.Decoder Item
itemDecoder =
    XD.succeed Item
        |> XDP.requiredPath [ "ASIN" ] (XD.single XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "Title" ] (XD.single XD.string)
        |> XDP.requiredPath [ "ItemAttributes", "ReleaseDate" ] (XD.single XD.date)
        |> XDP.requiredPath [ "ItemAttributes", "Author" ] (XD.list XD.string)
        |> XDP.optionalPath [ "ItemAttributes", "Publisher" ] (XD.single XD.string) "N/A"
        |> XD.map2 (|>) linksDecoder


linksDecoder : XD.Decoder Links
linksDecoder =
    XD.succeed Links
        |> XDP.requiredPath [ "DetailPageURL" ] (XD.single XD.string)
        |> XDP.optionalPath [ "SmallImage", "URL" ] (XD.single XD.string) "https://example.com/assets/image/fallback/small.png"
        |> XDP.optionalPath [ "MediumImage", "URL" ] (XD.single XD.string) "https://example.com/assets/image/fallback/medium.png"
        |> XDP.optionalPath [ "LargeImage", "URL" ] (XD.single XD.string) "https://example.com/assets/image/fallback/large.png"


searchParams : BrowseNode -> Sort -> Int -> String -> List String -> KVS
searchParams browseNode sort_ page publisher keywords =
    [ "Operation" => "ItemSearch"
    , "SearchIndex" => "Books" -- Required for Power Search
    , "ResponseGroup" => "Request,ItemAttributes,Images"
    , "BrowseNode" => browseNodeId browseNode
    , "Sort" => sort sort_
    , "ItemPage" => toString page
    , "Power" => power publisher keywords
    ]


power : String -> List String -> String
power publisher keywords =
    [ "not 分冊"
    , "not 雑誌"
    , "not 画集"
    , "not まとめ買い"
    , "pubdate: during 11-2017"
    , "publisher: " ++ publisher
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
        XD.single <|
            (XD.succeed browseNodeLookupResponse
                |> XDP.requiredPath [ "BrowseNodeId" ] (XD.single XD.string)
                |> XDP.requiredPath [ "Name" ] (XD.single XD.string)
                |> XDP.optionalPath [ "Children", "BrowseNode" ] (XD.list adjacentBrowseNodeDecoder) []
                |> XDP.optionalPath [ "Ancestors", "BrowseNode" ] (XD.list adjacentBrowseNodeDecoder) []
            )


adjacentBrowseNodeDecoder : XD.Decoder AdjacentBrowseNode
adjacentBrowseNodeDecoder =
    XD.succeed AdjacentBrowseNode
        |> XDP.requiredPath [ "BrowseNodeId" ] (XD.single XD.string)
        |> XDP.requiredPath [ "Name" ] (XD.single XD.string)


browseNodeParams : BrowseNode -> KVS
browseNodeParams browseNode =
    [ "BrowseNodeId" => browseNodeId browseNode
    , "Operation" => "BrowseNodeLookup"
    ]
