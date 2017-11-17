module Http_ exposing (getXml)

import Http
import XmlParser
import Xml.Decode as XD


{-| XML version of `Http.get`
-}
getXml : String -> XD.Decoder a -> Http.Request a
getXml url decoder =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/xml" ]
        , url = url
        , body = Http.emptyBody
        , expect = expectXml decoder
        , timeout = Nothing
        , withCredentials = False
        }


expectXml : XD.Decoder a -> Http.Expect a
expectXml =
    parseAndDecodeXml >> Http.expectStringResponse


parseAndDecodeXml : XD.Decoder a -> Http.Response String -> Result String a
parseAndDecodeXml decoder { body } =
    case XmlParser.parse body of
        Ok xml ->
            case XD.decodeXml decoder xml of
                Ok decoded ->
                    Ok decoded

                Err error ->
                    Err <| XD.errorToString error

        Err parserError ->
            -- Could use disassembling Parser.Error into more readable string here; though we skip it for now
            Err <| toString parserError
