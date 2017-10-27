module Xml exposing (dig, q, text)

import XmlParser exposing (Xml, Node(..))


dig : List String -> Xml -> Maybe (List Node)
dig path { root } =
    digImpl path root


digImpl : List String -> Node -> Maybe (List Node)
digImpl path node =
    case path of
        [] ->
            Just (children node)

        k :: ks ->
            case List.foldl (digReducer k) Nothing (children node) of
                Just n1 ->
                    digImpl ks n1

                Nothing ->
                    Nothing


children : Node -> List Node
children node =
    case node of
        Element _ _ nodes ->
            nodes

        Text _ ->
            []


digReducer : String -> Node -> Maybe Node -> Maybe Node
digReducer key node acc =
    case acc of
        Just _ ->
            acc

        Nothing ->
            case node of
                Element name _ _ ->
                    if name == key then
                        Just node
                    else
                        Nothing

                Text _ ->
                    Nothing


{-| Extract flattened list of nodes/values that matches specified path.

Paths can be given in XPath-like syntax:

  - `["plain", "tag", "name", "list"]`
      - Returns list of `XmlParser.Element`s with matching tag names.
  - `["tag", "names", "followed", "by", "$text"]`
      - Returns list of `XmlParser.Text`s at specified path.

`@someAttribute` syntax is yet to be supported.

-}
q : List String -> Xml -> List Node
q path { root } =
    qImpl path <| children root


qImpl : List String -> List Node -> List Node
qImpl path nodes =
    case path of
        [] ->
            nodes

        [ k ] ->
            nodes
                |> List.filter (hasName k)

        k :: ks ->
            nodes
                |> List.filter (hasName k)
                |> List.concatMap children
                |> qImpl ks


hasName : String -> Node -> Bool
hasName name node =
    case node of
        Element nodeName _ _ ->
            name == nodeName

        Text _ ->
            name == "$text"


{-| Extract bare `String` from `XmlParser.Node`, somewhat unsafely.

  - If the node is `XmlParser.Text`, extract its value.
  - If the node is `XmlParser.Element` AND contains a single `XmlParser.Text` child,
    extract its value.
  - Otherwise returns empty string.

Due to the third feature, this function should mainly be used for logging purpose.
Since it will not discriminate actual empty string in `XmlParser.Text` node
and `XmlParser.Element` node without an immediate string value.

-}
text : Node -> String
text node =
    case node of
        Text str ->
            str

        Element _ _ [ Text str ] ->
            str

        _ ->
            ""
