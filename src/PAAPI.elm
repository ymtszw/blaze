module PAAPI exposing (Credentials, Locale(..), Request, Error(..), get, signedUrl, sign)

{-| Amazon Product Advertising API (PAAPI) Client module.

Performs AWS V2 signing for request authentication.

<http://docs.aws.amazon.com/AWSECommerceService/latest/DG/Query_QueryAuth.html>

In order to do that, it accepts initial request parameters in
`Request` record, and client credentials in `Credentials` record.
It uses GET request with query strings in URL.

-}

import Regex
import Time exposing (Time)
import Http
import Crypto.HMAC exposing (sha256)
import Word.Bytes as Bytes
import BinaryBase64
import Date
import Date.Extra
import XmlParser exposing (Xml)
import Util exposing (KVS, (=>))
import Xml.Decode as XD


type Locale
    = BR
    | CA
    | CN
    | FR
    | DE
    | IN
    | IT
    | JP
    | MX
    | ES
    | UK
    | US


type alias Credentials =
    { accessKeyId : String
    , secretAccessKey : String
    , associateTag : String
    }


type alias Request =
    { locale : Locale
    , params : KVS
    }


paapiPath : String
paapiPath =
    "/onca/xml"


endpoint : Locale -> String
endpoint locale =
    case locale of
        BR ->
            "webservices.amazon.com.br"

        CA ->
            "webservices.amazon.ca"

        CN ->
            "webservices.amazon.cn"

        FR ->
            "webservices.amazon.fr"

        DE ->
            "webservices.amazon.de"

        IN ->
            "webservices.amazon.in"

        IT ->
            "webservices.amazon.it"

        JP ->
            "webservices.amazon.co.jp"

        MX ->
            "webservices.amazon.com.mx"

        ES ->
            "webservices.amazon.es"

        UK ->
            "webservices.amazon.co.uk"

        US ->
            "webservices.amazon.com"


type Error
    = Limit
    | InvalidBody String
    | Fail Http.Error


get : Credentials -> XD.Decoder a -> (Result Error a -> msg) -> Time -> Request -> Cmd msg
get creds decoder msg time req =
    let
        timestamp =
            time |> Date.fromTime |> Date.Extra.toUtcIsoString

        newReq =
            { req | params = ( "Timestamp", timestamp ) :: req.params }
    in
        signedUrl creds newReq
            |> Http.getString
            |> Http.send (mapResult decoder msg)


mapResult : XD.Decoder a -> (Result Error a -> msg) -> Result Http.Error String -> msg
mapResult decoder msg result =
    case result of
        Ok str ->
            case XmlParser.parse str of
                Ok xml ->
                    case XD.decodeXml decoder xml of
                        Ok decoded ->
                            msg <| Ok decoded

                        Err error ->
                            msg <| Err <| InvalidBody error

                Err parserError ->
                    msg <| Err <| InvalidBody <| toString parserError

        Err httpError ->
            msg <| Err <| handleHttpError httpError


handleHttpError : Http.Error -> Error
handleHttpError error =
    case error of
        Http.BadStatus { status } ->
            case status.code of
                503 ->
                    Limit

                _ ->
                    Fail error

        _ ->
            Fail error


{-| Generates signed URL for PAAPI.
-}
signedUrl : Credentials -> Request -> String
signedUrl creds req =
    let
        ( ep, cp, signature ) =
            sign creds req
    in
        "http://"
            ++ ep
            ++ paapiPath
            ++ ("?" ++ cp)
            ++ ("&Signature=" ++ urlEscape signature)


{-| Generates AWS Request Signature V2 for PAAPI.
-}
sign : Credentials -> Request -> ( String, String, String )
sign creds { locale, params } =
    let
        ep =
            endpoint locale

        cp =
            canonicalParams (requiredParams ++ credentialParams creds ++ params)

        signature =
            canonicalRequest ep cp
                |> Bytes.fromUTF8
                |> Crypto.HMAC.digestBytes sha256 (Bytes.fromUTF8 creds.secretAccessKey)
                |> BinaryBase64.encode
    in
        ( ep, cp, signature )


canonicalRequest : String -> String -> String
canonicalRequest endpoint canonicalParams =
    "GET\n"
        ++ (endpoint ++ "\n")
        ++ (paapiPath ++ "\n")
        ++ canonicalParams


credentialParams : Credentials -> KVS
credentialParams { accessKeyId, associateTag } =
    [ "AWSAccessKeyId" => accessKeyId
    , "AssociateTag" => associateTag
    ]


canonicalParams : KVS -> String
canonicalParams params =
    params
        |> List.map (\( k, v ) -> urlEscape k ++ "=" ++ urlEscape v)
        |> List.sort
        |> String.join "&"


requiredParams : KVS
requiredParams =
    [ "Service" => "AWSECommerceService"
    , "Version" => "2013-08-01" -- Optional, though include it for locking
    ]


{-| Escapes strings per AWS's request signing standard (both V2 and current V4).

It basicaly uses `encodeURIComponent` of JavaScript (via `Http.encodeUri`),
though additionaly, replaces '*' to '%20'.

See [here](http://docs.aws.amazon.com/AWSECommerceService/latest/DG/Query_QueryAuth.html)
for PAAPI authenticaton requirements (V2 signing).
And [here](https://github.com/aws/aws-sdk-js/blob/master/lib/util.js#L41)
for aws-sdk implementation.

-}
urlEscape : String -> String
urlEscape str =
    str
        |> Http.encodeUri
        |> Regex.replace Regex.All (Regex.regex (Regex.escape "*")) (\_ -> "%20")
