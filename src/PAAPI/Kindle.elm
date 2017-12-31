module PAAPI.Kindle
    exposing
        ( BrowseNode(..)
        , Sort(..)
        , Item
        , Response(..)
        , toBrowseNode
        , search
        , browseNodeLookup
        , itemLookup
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
import Task exposing (Task)
import Rocket exposing ((=>))
import Util exposing (KVS)
import Xml.Decode as XD
import Xml.Decode.Extra exposing ((|:))
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
    = Search SearchResponse
    | BrowseNodeLookup BrowseNodeLookupResponse
    | ItemLookup (List Item)


type alias SearchResponse =
    { totalPages : Int
    , currentPage : Int
    , items : List Item
    }


type alias BrowseNodeLookupResponse =
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



-- Search


{-| Searches items with given parameters.
-}
search :
    PAAPI.Credentials
    -> PAAPI.AssociateTag
    -> BrowseNode
    -> Sort
    -> Int
    -> List String
    -> Task PAAPI.Error Response
search creds tag browseNode sort_ page powerSearchParams =
    PAAPI.doGet PAAPI.JP
        creds
        tag
        searchResponseDecoder
        (searchParams browseNode
            sort_
            page
            powerSearchParams
        )


searchResponseDecoder : XD.Decoder Response
searchResponseDecoder =
    XD.map Search <|
        XD.path [ "Items" ] <|
            XD.single <|
                (XD.succeed SearchResponse
                    |: XD.path [ "TotalPages" ] (XD.single XD.int)
                    |: XD.withDefault 1 (XD.path [ "Request", "ItemSearchRequest", "ItemPage" ] (XD.single XD.int))
                    |: XD.path [ "Item" ] (XD.list itemDecoder)
                )


itemDecoder : XD.Decoder Item
itemDecoder =
    XD.succeed Item
        |: XD.path [ "ASIN" ] (XD.single XD.string)
        |: XD.path [ "ItemAttributes", "Title" ] (XD.single XD.string)
        |: XD.path [ "ItemAttributes", "ReleaseDate" ] (XD.single XD.date)
        |: XD.path [ "ItemAttributes", "Author" ] (XD.list XD.string)
        |: XD.withDefault "N/A" (XD.path [ "ItemAttributes", "Publisher" ] (XD.single XD.string))
        |: linksDecoder


linksDecoder : XD.Decoder Links
linksDecoder =
    XD.succeed Links
        |: XD.path [ "DetailPageURL" ] (XD.single XD.string)
        |: XD.withDefault "https://example.com/assets/image/fallback/small.png" (XD.path [ "SmallImage", "URL" ] (XD.single XD.string))
        |: XD.withDefault "https://example.com/assets/image/fallback/medium.png" (XD.path [ "MediumImage", "URL" ] (XD.single XD.string))
        |: XD.withDefault "https://example.com/assets/image/fallback/large.png" (XD.path [ "LargeImage", "URL" ] (XD.single XD.string))


searchParams : BrowseNode -> Sort -> Int -> List String -> KVS
searchParams browseNode sort_ page powerSearchParams =
    [ "Operation" => "ItemSearch"
    , "SearchIndex" => "Books" -- Required for Power Search
    , "ResponseGroup" => itemResponseGroup
    , "BrowseNode" => browseNodeId browseNode
    , "Sort" => sort sort_
    , "ItemPage" => toString page
    , "Power" => power powerSearchParams
    ]


itemResponseGroup : String
itemResponseGroup =
    "Request,ItemAttributes,Images"


power : List String -> String
power powerSearchParams =
    [ "not 分冊"
    , "not 雑誌"
    , "not 画集"
    , "not まとめ買い"
    ]
        |> (++) powerSearchParams
        |> String.join " and "



-- BrowseNodeLookup


{-| Retrieves existing BrowseNodes relative to the given `BrowseNode`.
-}
browseNodeLookup :
    PAAPI.Credentials
    -> PAAPI.AssociateTag
    -> BrowseNode
    -> Task PAAPI.Error Response
browseNodeLookup creds tag browseNode =
    PAAPI.doGet PAAPI.JP
        creds
        tag
        browseNodeLookupResponseDecoder
        (browseNodeParams browseNode)


browseNodeLookupResponseDecoder : XD.Decoder Response
browseNodeLookupResponseDecoder =
    XD.map BrowseNodeLookup <|
        XD.path [ "BrowseNodes", "BrowseNode" ] <|
            XD.single <|
                (XD.succeed BrowseNodeLookupResponse
                    |: XD.path [ "BrowseNodeId" ] (XD.single XD.string)
                    |: XD.path [ "Name" ] (XD.single XD.string)
                    |: XD.path [ "Children", "BrowseNode" ] (XD.list adjacentBrowseNodeDecoder)
                    |: XD.path [ "Ancestors", "BrowseNode" ] (XD.list adjacentBrowseNodeDecoder)
                )


adjacentBrowseNodeDecoder : XD.Decoder AdjacentBrowseNode
adjacentBrowseNodeDecoder =
    XD.succeed AdjacentBrowseNode
        |: XD.path [ "BrowseNodeId" ] (XD.single XD.string)
        |: XD.path [ "Name" ] (XD.single XD.string)


browseNodeParams : BrowseNode -> KVS
browseNodeParams browseNode =
    [ "BrowseNodeId" => browseNodeId browseNode
    , "Operation" => "BrowseNodeLookup"
    ]



-- ItemLookup


itemLookup : PAAPI.Credentials -> PAAPI.AssociateTag -> List String -> Task PAAPI.Error Response
itemLookup creds tag asins =
    PAAPI.doGet PAAPI.JP
        creds
        tag
        itemLookupResponseDecoder
        (itemLookupParams asins)


itemLookupResponseDecoder : XD.Decoder Response
itemLookupResponseDecoder =
    XD.map ItemLookup <|
        XD.path [ "Items", "Item" ] (XD.list itemDecoder)


itemLookupParams : List String -> KVS
itemLookupParams asins =
    [ "Operation" => "ItemLookup"
    , "ResponseGroup" => itemResponseGroup
    , "ItemId" => (asins |> List.take 10 |> String.join ",")
    ]
