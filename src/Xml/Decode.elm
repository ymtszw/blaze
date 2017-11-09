module Xml.Decode
    exposing
        ( Decoder
        , ListDecoder
        , decodeXml
        , string
        , int
        , float
        , bool
        , date
        , succeed
        , fail
        , singleton
        , list
        , leakyList
        , andThen
        , map2
        , withDefault
        , maybe
        , lazy
        , path
        )

{-| Xml decoder module sharing the spirit of `Json.Decode`.

Due to the nature of XML, you cannot distinguish a particular tag or tags hold
whether "singular value" or "list of values", from the structure of XML document itself.
Since there can be multiple tags with the same name in a same level.

This is contrary to the JSON, where there can be only one field of a paticular key in a level,
and its quantization is obvious from the shape of the value:
must be a list if it is `[<value>, ...]`, otherwise singular.

For the sake of consistency, our primary "query" function, `path`,
will always produce list of `Node`s.
Then they must be decoded using special decoder parts: `singleton` and `list`.

They are used in conjunction with `Decoder`s like so:

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        map2 SomeRecord
            (path [ "path", "to", "string", "value" ] (singleton string))
            (path [ "path", "to", "int", "values" ] (list int))

If you want to perform complex resolution of multiple matches from `path`,
you can implement variants of `singleton` and `list`: `ListDecoder`s.

-}

import Date exposing (Date)
import XmlParser exposing (Xml, Node(..))
import Xml


-- TYPES


type alias Decoder a =
    Node -> Result String a


type alias ListDecoder a =
    List Node -> Result String a



-- DECODE EXECUTOR


{-| Parses an `XmlParser.Xml` data into other type of Elm data, using `Decoder`.
-}
decodeXml : Decoder a -> Xml -> Result String a
decodeXml decoder { root } =
    decoder root



-- DECODER ELEMENTS


{-| Decodes a `XmlParser.Node` into `String`.

  - If the node is `XmlParser.Text`, extracts its value.
  - If the node is `XmlParser.Element` AND contains a single `XmlParser.Text` child,
    extracts its value.
  - Otherwise fails.

-}
string : Decoder String
string node =
    case node of
        Text str ->
            Ok str

        Element _ _ [ Text str ] ->
            Ok str

        _ ->
            Err <| "The node is not a simple text node. Got: " ++ (toString node)


{-| Similar to `string`, but also tries to convert `String` to `Int`.
-}
int : Decoder Int
int =
    string >> Result.andThen String.toInt


{-| Decodes to `Float`.
-}
float : Decoder Float
float =
    string >> Result.andThen String.toFloat


{-| Decodes to `Bool`.

In Xml Schema Definition (XSD), valid lexical representation of boolean values are,

  - 'true'
  - 'false'
  - '1'
  - '0'

We follow this specification, case-sensitively.

<https://www.w3.org/TR/xmlschema-2/#boolean>

-}
bool : Decoder Bool
bool =
    let
        toBool str =
            case str of
                "true" ->
                    Ok True

                "1" ->
                    Ok True

                "false" ->
                    Ok False

                "0" ->
                    Ok False

                _ ->
                    Err <| "Not a valid boolean value. Got: " ++ str
    in
        string >> Result.andThen toBool


{-| Decodes to `Date`.

It uses `new Date()` of JavaScript under the hood.

<https://github.com/elm-lang/core/blob/5.1.1/src/Native/Date.js#L5>

-}
date : Decoder Date
date =
    string >> Result.andThen Date.fromString


{-| Decoder that always succeed with the given value.
-}
succeed : a -> Decoder a
succeed a =
    always (Ok a)


{-| Decoder that always fail with the given message.
-}
fail : String -> Decoder a
fail error =
    always (Err error)



-- LIST DECODERS


{-| Composes `ListDecoder` that results in a singular value.

It fails if:

  - there are multiple nodes, or,
  - there are no nodes.

-}
singleton : Decoder a -> ListDecoder a
singleton decoder nodes =
    case nodes of
        [] ->
            Err "Nodes not found."

        [ singleton_ ] ->
            decoder singleton_

        _ :: _ ->
            Err <| "Multiple nodes found. Got: " ++ toString nodes


{-| Composes `ListDecoder` that results in a list of values.

This `ListDecoder` fails if any incoming items cannot be decoded.

-}
list : Decoder a -> ListDecoder (List a)
list decoder =
    List.foldr (decoder >> Result.map2 (::)) (Ok [])


{-| Variation of `list`, which ignores items that cannot be decoded.
-}
leakyList : Decoder a -> ListDecoder (List a)
leakyList decoder =
    List.foldr (decoder >> accumlateOk) (Ok [])


accumlateOk : Result x a -> Result x (List a) -> Result x (List a)
accumlateOk result acc =
    case result of
        Err _ ->
            acc

        Ok a ->
            Result.map ((::) a) acc



-- DECODER UTILITY


{-| Generates a decoder that depends on previous value.
-}
andThen : (a -> Decoder b) -> Decoder a -> Decoder b
andThen decoderBGen decoderA node =
    case decoderA node of
        Ok valA ->
            node |> decoderBGen valA

        Err e ->
            Err e


{-| Takes two decoders, then generates a decoder that combines results from those decoders.

It can be used for generating a decoder for a data type that takes two inputs.
Although mainly, this is used as a building block of DSL style decoder generation.

Also see `Xml.Decode.Pipeline` or `Xml.Decode.Extra`.

-}
map2 : (a -> b -> value) -> Decoder a -> Decoder b -> Decoder value
map2 valueGen decoderA decoderB xml =
    Result.map2 valueGen
        (decoderA xml)
        (decoderB xml)


{-| Generates a decoder that results in the default value on failure.
-}
withDefault : a -> Decoder a -> Decoder a
withDefault default decoder =
    let
        applyDefault result =
            case result of
                Ok _ ->
                    result

                Err _ ->
                    Ok default
    in
        decoder >> applyDefault


{-| Generates a decoder that results in a `Maybe` value.

If the given decoder resulted in `Err`, it succeeds with `Nothing`.
Otherwise (in cases of `Ok`,) it succeeds with `Just` value.

-}
maybe : Decoder a -> Decoder (Maybe a)
maybe decoder =
    let
        maybify result =
            case result of
                Ok val ->
                    Ok (Just val)

                Err _ ->
                    Ok (Nothing)
    in
        decoder >> maybify


{-| Generates a lazy decoder.

Similar to `Json.Decode.lazy`.

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        map2 SomeRecord
            (path [ "path", "to", "string", "value" ] (singleton string))
            (path [ "path", "to", "list", "of", "someRecord" ] (list lazy (\_ -> someRecordDecoder)))

With this, you can avoid "bad-recursion" error on compilation
which happens when you define nested part of the above decoder as `(list someRecordDecoder)`

-}
lazy : (() -> Decoder a) -> Decoder a
lazy =
    flip andThen (succeed ())


{-| Generates a decoder that applies a `ListDecoder` at specified XML path.

Typical usage:

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        map2 SomeRecord
            (path [ "path", "to", "string", "value" ] (singleton string))
            (path [ "path", "to", "int", "values" ] (list int))

Note that in the `path_` list, you must start from within the root scope.
For instance, to work with an XML document like:

    <Root>
        <Path>
            <Target>Value</Target>
        </Path>
    </Root>

You should specify:

    path ["Path", "Target"] (singleton string)

-}
path : List String -> ListDecoder a -> Decoder a
path path_ listDecoder =
    Xml.children >> Xml.query path_ >> listDecoder >> putPathOnError path_


putPathOnError : List String -> Result String a -> Result String a
putPathOnError path_ result =
    case result of
        Ok _ ->
            result

        Err originalError ->
            Err <| originalError ++ " At: " ++ toString path_
