module Igniter.Seed exposing (..)

{-| Seeding tools for populating item database.


## BrowseNode

BrowseNode can be specified by the dedicated query parameter.

Comics of JP Kindle store are classified by four primary BrowseNodes:
Boys, Mens, Girls and Ladies.

Igniter basically walks through these four BrowseNodes.


## Publication Date

Publication Date query can be specified by "pubdate" keyword
with "during" or "after" operators in Power search.

Do note that this query can only match against items
with PublicationDate attribute registered.

Basically, Publication Dates are dates when the items are published
in printed books, not e-books.
Release dates of e-books can be found in ReleaseDate attributes.


## Publisher

Similar to PublicationDate, items can be queried with "publisher" keyword
in Power search.

Some items do not have Publisher attribute.

-}

import Date exposing (Month(..))
import Set exposing (Set)
import Fuse
import Util
import PAAPI.Kindle exposing (BrowseNode(..))


-- BrowseNode


comicNodes : List BrowseNode
comicNodes =
    [ Boys, Mens, Girls, Ladies ]



-- Publication Date


type Pubdate
    = Pubdate Month Int


pubdateOrigin : Pubdate
pubdateOrigin =
    Pubdate Jan 2018


nextPubdate : Pubdate -> Pubdate
nextPubdate (Pubdate m y) =
    case ( m, y ) of
        ( Dec, y ) ->
            Pubdate Jan (y + 1)

        ( m, y ) ->
            Pubdate (nextM m) y


nextM : Month -> Month
nextM m =
    case m of
        Jan ->
            Feb

        Feb ->
            Mar

        Mar ->
            Apr

        Apr ->
            May

        May ->
            Jun

        Jun ->
            Jul

        Jul ->
            Aug

        Aug ->
            Sep

        Sep ->
            Oct

        Oct ->
            Nov

        Nov ->
            Dec

        Dec ->
            Jan


prevPubdate : Pubdate -> Pubdate
prevPubdate (Pubdate m y) =
    case ( m, y ) of
        ( Jan, y ) ->
            Pubdate Dec (y - 1)

        ( m, y ) ->
            Pubdate (prevM m) y


prevM : Month -> Month
prevM m =
    case m of
        Jan ->
            Dec

        Feb ->
            Jan

        Mar ->
            Feb

        Apr ->
            Mar

        May ->
            Apr

        Jun ->
            May

        Jul ->
            Jun

        Aug ->
            Jul

        Sep ->
            Aug

        Oct ->
            Sep

        Nov ->
            Oct

        Dec ->
            Nov



-- Publisher


type PublisherFilter
    = By String
    | Exclude (Set String)


wellknownPublishers : Set String
wellknownPublishers =
    Set.fromList
        [ kodansha
        , shueisha
        , shogakukan
        , kadokawa
        ]


kodansha : String
kodansha =
    "講談社"


shueisha : String
shueisha =
    "集英社"


shogakukan : String
shogakukan =
    "小学館"


kadokawa : String
kadokawa =
    "KADOKAWA*"



-- Power Search


power : Maybe Pubdate -> PublisherFilter -> List String
power maybePubdate publisherFilter =
    [ Maybe.map pubdateParam maybePubdate
    , publisherParam publisherFilter
    ]
        |> List.filterMap identity


pubdateParam : Pubdate -> String
pubdateParam (Pubdate month year) =
    "pubdate:during "
        ++ (String.padLeft 2 '0' <| toString <| Util.monthToInt month)
        ++ "-"
        ++ (toString year)


publisherParam : PublisherFilter -> Maybe String
publisherParam publisherFilter =
    let
        exclude p =
            "(not publisher: " ++ p ++ ")"
    in
        case publisherFilter of
            By p ->
                Just <| "publisher: " ++ p

            Exclude ps ->
                case ps |> Set.toList |> List.map exclude |> String.join " and " of
                    "" ->
                        Nothing

                    filter ->
                        Just filter



-- Collect Publishers


dumpCollectedPublishers : Set String -> Cmd msg
dumpCollectedPublishers publishers =
    publishers |> Set.toList |> String.join "\n" |> curry Fuse.writeFile "known_publishers"
