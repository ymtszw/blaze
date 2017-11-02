module Xml.Decode.Extra exposing ((|:))

{-| Xml decoder module sharing the spirit of `Json.Decode.Extra`.

Uses `(|:)` infix operator that allows writing decoders in DSL style like so:

    import Xml.Decode exposing (succeed, path, singleton, string)

    someRecordDecoder : Decoder SomeRecord
    someRecordDecoder =
        succeed SomeRecord
            |: path [ "path", "to", "textField1" ] (singleton string)
            |: path [ "path", "to", "textField2" ] (singleton string)

Benefit of this style is it leverages standard decoders from `Xml.Decode`,
so you only have to import `(|:)`.

-}

import Xml.Decode exposing (Decoder)


andMap : Decoder a -> Decoder (a -> b) -> Decoder b
andMap =
    Xml.Decode.map2 (|>)


{-| Equivalent to `Json.Decode.Extra.(|:)`, allows writing XML decoders in DSL style.
-}
(|:) : Decoder (a -> b) -> Decoder a -> Decoder b
(|:) =
    flip andMap
