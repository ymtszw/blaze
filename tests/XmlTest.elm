module XmlTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Fuzz exposing (..)
import XmlParser exposing (Node(..))
import Util
import Xml
import Xml.Decode as XD exposing (Decoder)


suite : Test
suite =
    describe "Xml"
        [ test "`dig` should extract child nodes of targeted path (only for the first match)" <|
            \_ ->
                [ [ "NonExistent" ]
                , [ "OperationRequest", "RequestId" ]
                , [ "Items", "Request", "ItemSearchRequest" ]
                , [ "Items", "Item", "ItemAttributes", "Title" ]
                ]
                    |> List.map (flip Xml.dig <| xml exampleXmlElement)
                    |> equalLists
                        [ Nothing
                        , Just [ Text "93498126-da8f-4d62-945f-d6719e8c2a25" ]
                        , Just
                            [ Element "BrowseNode" [] [ Text "2250739051" ]
                            , Element "Keywords" [] [ Text "衿沢世衣子" ]
                            , Element "ResponseGroup" [] [ Text "Small" ]
                            , Element "SearchIndex" [] [ Text "KindleStore" ]
                            , Element "Sort" [] [ Text "daterank" ]
                            ]
                        , Just [ Text "モーニング 2017年34号 [2017年7月20日発売] [雑誌]" ]
                        ]
        , test "`q` should extract whole list of nodes that matches path" <|
            \_ ->
                [ [ "NonExistent" ]
                , [ "OperationRequest", "RequestId" ]
                , [ "Items", "Item", "ItemAttributes", "Title" ]
                , [ "Items", "Item", "ItemAttributes", "Title", "$text" ]
                , [ "Items", "Item", "ItemAttributes", "Creator" ]
                ]
                    |> List.map (flip Xml.q <| xml exampleXmlElement)
                    |> equalLists
                        [ []
                        , [ Element "RequestId" [] [ Text "93498126-da8f-4d62-945f-d6719e8c2a25" ] ]
                        , [ Element "Title" [] [ Text "モーニング 2017年34号 [2017年7月20日発売] [雑誌]" ]
                          , Element "Title" [] [ Text "『デビルズライン』アニメ化記念\x3000恋する人外\x3000試し読み無料パック" ]
                          , Element "Title" [] [ Text "うちのクラスの女子がヤバい\x3000分冊版（１２）\x3000「リュウとランタンの灯り」 少年マガジンエッジコミックス" ]
                          ]
                        , [ Text "モーニング 2017年34号 [2017年7月20日発売] [雑誌]"
                          , Text "『デビルズライン』アニメ化記念\x3000恋する人外\x3000試し読み無料パック"
                          , Text "うちのクラスの女子がヤバい\x3000分冊版（１２）\x3000「リュウとランタンの灯り」 少年マガジンエッジコミックス"
                          ]
                        , [ Element "Creator" [ { name = "Role", value = "編集" } ] [ Text "モーニング編集部" ] ] -- Resulting lists should be flattened
                        ]
        , describe "`Decode`"
            [ describe "`Decoder`s and `decodeXml`"
                [ describe "`string`"
                    [ fuzz string "should decode string" <|
                        \str ->
                            [ Text str
                            , Element "tag" [] [ Text str ]
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml XD.string)
                                |> mustBeAllOk str
                    , test "should reject `Element` with non-singular children or non-`Text` child" <|
                        \_ ->
                            [ Element "tag" [] []
                            , Element "tag" [] [ Element "innerTag" [] [ Text "nestedText" ] ]
                            , Element "tag" [] [ Text "multiple", Element "elements" [] [] ]
                            , Element "tag" [] [ Element "multiple" [] [], Text "elements" ]
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml XD.string)
                                |> mustBeAllError
                    ]
                , describe "`int`"
                    [ fuzz int "should decode integer" <|
                        \i ->
                            [ Text <| toString i
                            , Element "tag" [] [ Text <| toString i ]
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml XD.int)
                                |> mustBeAllOk i
                    , test "should reject non-integer" <|
                        \_ ->
                            [ Text "string"
                            , Text "1.0"
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml XD.int)
                                |> mustBeAllError
                    ]
                , describe "`float`"
                    [ fuzz2 int float "should decode float (or int into float)" <|
                        \i f ->
                            [ Text <| toString i
                            , Text <| toString f
                            , Element "tag" [] [ Text <| toString i ]
                            , Element "tag" [] [ Text <| toString f ]
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml XD.float)
                                |> equalLists
                                    [ Ok (toFloat i)
                                    , Ok f
                                    , Ok (toFloat i)
                                    , Ok f
                                    ]
                    , test "should reject non-float-convertible" <|
                        \_ ->
                            Text "string"
                                |> xml
                                |> XD.decodeXml XD.float
                                |> err
                    ]
                , describe "`bool`"
                    [ test "should decode boolean" <|
                        \_ ->
                            [ Text "true"
                            , Text "1"
                            , Text "false"
                            , Text "0"
                            , Element "tag" [] [ Text "true" ]
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml XD.bool)
                                |> equalLists
                                    [ Ok True
                                    , Ok True
                                    , Ok False
                                    , Ok False
                                    , Ok True
                                    ]
                    , fuzz (stringWithout [ "true", "false", "0", "1" ]) "should reject non-boolean" <|
                        \nonBool ->
                            Text nonBool
                                |> xml
                                |> XD.decodeXml XD.bool
                                |> err
                    ]
                , describe "`path` with `singleton` and `list`"
                    [ describe "`singleton`"
                        [ test "should extract value from root element" <|
                            \_ ->
                                Element "root" [] [ Text "value" ]
                                    |> xml
                                    |> XD.decodeXml (XD.path [] <| XD.singleton XD.string)
                                    |> equal (Ok "value")
                        , test "should extract value from targeted node" <|
                            \_ ->
                                [ ( Element "root" [] [ Element "tag" [] [ Text "value" ] ], [ "tag" ] )
                                , ( Element "root" [] [ Element "tag1" [] [ Element "tag2" [] [ Text "value" ] ] ], [ "tag1", "tag2" ] )
                                , ( Element "root" [] [ Element "sametag" [] [ Element "sametag" [] [ Text "value" ] ] ], [ "sametag", "sametag" ] )
                                ]
                                    |> List.map (\( node, path ) -> ( xml node, XD.path path <| XD.singleton XD.string ))
                                    |> List.map (uncurry <| flip XD.decodeXml)
                                    |> mustBeAllOk "value"
                        , test "should fail for paths that do not yield nodes" <|
                            \_ ->
                                [ [ "nonexisting" ]
                                , [ "tag1", "wrongLeaf" ]
                                , [ "wrongTrunk", "tag2" ]
                                ]
                                    |> List.map (flip XD.path <| XD.singleton XD.string)
                                    |> List.map (flip XD.decodeXml <| xml <| Element "root" [] [ Element "tag1" [] [ Element "tag2" [] [ Text "value" ] ] ])
                                    |> mustBeAllError
                        , test "should fail for path with multiple matching nodes" <|
                            \_ ->
                                [ Element "tag" [] [ Text "value1" ]
                                , Element "tag" [] [ Text "value2" ]
                                ]
                                    |> (xml << Element "root" [])
                                    |> XD.decodeXml (XD.path [ "tag" ] <| XD.singleton XD.string)
                                    |> err
                        ]
                    , describe "`list`"
                        [ test "should extract values from targeted nodes" <|
                            \_ ->
                                [ [ Element "tag" [] [ Text "value1" ], Element "tag" [] [ Text "value2" ] ]
                                , [ Element "tag" [] [ Text "value1" ] ]
                                , []
                                ]
                                    |> List.map (xml << Element "root" [])
                                    |> List.map (XD.decodeXml <| XD.path [ "tag" ] <| XD.list XD.string)
                                    |> equalLists
                                        [ Ok [ "value1", "value2" ]
                                        , Ok [ "value1" ]
                                        , Ok []
                                        ]
                        ]
                    ]
                , describe "`withDefault`"
                    [ test "should handle value decoding failure" <|
                        \_ ->
                            Text "nonBool"
                                |> xml
                                |> XD.decodeXml (XD.withDefault True XD.bool)
                                |> equal (Ok True)
                    , test "should handle path finding failure" <|
                        \_ ->
                            Element "root" [] [ Element "tag" [] [ Text "value" ] ]
                                |> xml
                                |> XD.decodeXml (XD.withDefault "default" <| XD.path [ "nonexisting" ] <| XD.singleton XD.string)
                                |> equal (Ok "default")
                    ]
                , describe "`maybe`"
                    [ test "should handle value decoding failure" <|
                        \_ ->
                            [ Text "true"
                            , Text "nonBool"
                            ]
                                |> List.map xml
                                |> List.map (XD.decodeXml <| XD.maybe XD.bool)
                                |> equalLists
                                    [ Ok (Just True)
                                    , Ok Nothing
                                    ]
                    , test "should handle path finding failure" <|
                        \_ ->
                            Element "root" [] [ Element "tag" [] [ Text "value" ] ]
                                |> xml
                                |> XD.decodeXml (XD.maybe <| XD.path [ "nonexisting" ] <| XD.singleton XD.string)
                                |> equal (Ok Nothing)
                    ]
                ]
            ]
        ]


stringWithout : List String -> Fuzzer String
stringWithout excluded =
    flip conditional
        string
        { retries = 5
        , fallback = always ""
        , condition = flip List.member excluded >> not
        }


mustBeAllOk : b -> List (Result a b) -> Expectation
mustBeAllOk val results =
    if List.all ((==) (Ok val)) results then
        pass
    else
        fail <| toString results


mustBeAllError : List (Result a b) -> Expectation
mustBeAllError results =
    if List.all Util.isErr results then
        pass
    else
        fail <| toString results


xml : Node -> XmlParser.Xml
xml rootNode =
    { processingInstructions = []
    , docType = Nothing
    , root = rootNode
    }


{-| Fetched from actual PAAPI (somewhat trimmed).
-}
exampleXmlElement : Node
exampleXmlElement =
    Element "ItemSearchResponse"
        [ { name = "xmlns", value = "http://webservices.amazon.com/AWSECommerceService/2013-08-01" } ]
        [ Element "OperationRequest"
            []
            [ Element "HTTPHeaders"
                []
                [ Element "Header" [ { name = "Name", value = "UserAgent" }, { name = "Value", value = "Mozilla/5.0 Darwin x64 node.js/6.11.4 v8/5.1.281.108" } ] []
                ]
            , Element "RequestId" [] [ Text "93498126-da8f-4d62-945f-d6719e8c2a25" ]
            , Element "Arguments"
                []
                [ Element "Argument" [ { name = "Name", value = "BrowseNode" }, { name = "Value", value = "2250739051" } ] []
                , Element "Argument" [ { name = "Name", value = "Keywords" }, { name = "Value", value = "衿沢世衣子" } ] []
                , Element "Argument" [ { name = "Name", value = "Operation" }, { name = "Value", value = "ItemSearch" } ] []
                , Element "Argument" [ { name = "Name", value = "SearchIndex" }, { name = "Value", value = "KindleStore" } ] []
                , Element "Argument" [ { name = "Name", value = "Service" }, { name = "Value", value = "AWSECommerceService" } ] []
                , Element "Argument" [ { name = "Name", value = "Sort" }, { name = "Value", value = "daterank" } ] []
                , Element "Argument" [ { name = "Name", value = "Timestamp" }, { name = "Value", value = "2017-10-25T14:31:43.823Z" } ] []
                , Element "Argument" [ { name = "Name", value = "Version" }, { name = "Value", value = "2013-08-01" } ] []
                , Element "Argument" [ { name = "Name", value = "Signature" }, { name = "Value", value = "zMQJMcfsSlkjwdYtWqxjgGZvEKtemsEA8onoyg/x1GE=" } ] []
                ]
            , Element "RequestProcessingTime" [] [ Text "0.0329217540000000" ]
            ]
        , Element "Items"
            []
            [ Element "Request"
                []
                [ Element "IsValid" [] [ Text "True" ]
                , Element "ItemSearchRequest"
                    []
                    [ Element "BrowseNode" [] [ Text "2250739051" ]
                    , Element "Keywords" [] [ Text "衿沢世衣子" ]
                    , Element "ResponseGroup" [] [ Text "Small" ]
                    , Element "SearchIndex" [] [ Text "KindleStore" ]
                    , Element "Sort" [] [ Text "daterank" ]
                    ]
                ]
            , Element "TotalResults" [] [ Text "48" ]
            , Element "TotalPages" [] [ Text "5" ]
            , Element "Item"
                []
                [ Element "ASIN" [] [ Text "B073XHZBSH" ]
                , Element "ItemAttributes"
                    []
                    [ Element "Author" [] [ Text "江口夏実" ]
                    , Element "Author" [] [ Text "守村大" ]
                    , Element "Author" [] [ Text "ツジトモ" ]
                    , Element "Author" [] [ Text "綱本将也" ]
                    , Element "Author" [] [ Text "東元俊哉" ]
                    , Element "Author" [] [ Text "森高夕次" ]
                    , Element "Author" [] [ Text "アダチケイジ" ]
                    , Element "Author" [] [ Text "アビディ井上" ]
                    , Element "Author" [] [ Text "鈴ノ木ユウ" ]
                    , Element "Author" [] [ Text "河部真道" ]
                    , Element "Author" [] [ Text "福田泰宏" ]
                    , Element "Author" [] [ Text "コージィ城倉" ]
                    , Element "Author" [] [ Text "ＰＥＡＣＨ－ＰＩＴ" ]
                    , Element "Author" [] [ Text "ラズウェル細木" ]
                    , Element "Author" [] [ Text "なきぼくろ" ]
                    , Element "Author" [] [ Text "よしながふみ" ]
                    , Element "Author" [] [ Text "山田芳裕" ]
                    , Element "Author" [] [ Text "衿沢世衣子" ]
                    , Element "Author" [] [ Text "かわぐちかいじ" ]
                    , Element "Author" [] [ Text "亜樹直" ]
                    , Element "Author" [] [ Text "オキモト・シュウ" ]
                    , Element "Author" [] [ Text "とりのなん子" ]
                    , Element "Author" [] [ Text "堀内厚徳" ]
                    , Element "Author" [] [ Text "土塚理弘" ]
                    , Element "Author" [] [ Text "田島隆" ]
                    , Element "Author" [] [ Text "東風孝広" ]
                    , Element "Author" [] [ Text "うえやまとち" ]
                    , Element "Author" [] [ Text "あらゐけいいち" ]
                    , Element "Creator" [ { name = "Role", value = "編集" } ] [ Text "モーニング編集部" ]
                    , Element "Manufacturer" [] [ Text "講談社" ]
                    , Element "ProductGroup" [] [ Text "eBooks" ]
                    , Element "Title" [] [ Text "モーニング 2017年34号 [2017年7月20日発売] [雑誌]" ]
                    ]
                ]
            , Element "Item"
                []
                [ Element "ASIN" [] [ Text "B073PWP7Y7" ]
                , Element "ItemAttributes"
                    []
                    [ Element "Author" [] [ Text "花田陵" ]
                    , Element "Author" [] [ Text "松浦だるま" ]
                    , Element "Author" [] [ Text "光永康則" ]
                    , Element "Author" [] [ Text "田口ホシノ" ]
                    , Element "Author" [] [ Text "久保保久" ]
                    , Element "Author" [] [ Text "道明宏明" ]
                    , Element "Author" [] [ Text "あだちとか" ]
                    , Element "Author" [] [ Text "鳥海ペドロ" ]
                    , Element "Author" [] [ Text "野切耀子" ]
                    , Element "Author" [] [ Text "車谷晴子" ]
                    , Element "Author" [] [ Text "ほおのきソラ" ]
                    , Element "Author" [] [ Text "衿沢世衣子" ]
                    , Element "Author" [] [ Text "樋口彰彦" ]
                    , Element "Author" [] [ Text "島袋全優" ]
                    , Element "Author" [] [ Text "硝音あや" ]
                    , Element "Author" [] [ Text "柚月純" ]
                    , Element "Manufacturer" [] [ Text "講談社" ]
                    , Element "ProductGroup" [] [ Text "eBooks" ]
                    , Element "Title" [] [ Text "『デビルズライン』アニメ化記念\x3000恋する人外\x3000試し読み無料パック" ]
                    ]
                ]
            , Element "Item"
                []
                [ Element "ASIN" [] [ Text "B0716SQ575" ]
                , Element "ItemAttributes"
                    []
                    [ Element "Author" [] [ Text "衿沢世衣子" ]
                    , Element "Manufacturer" [] [ Text "講談社" ]
                    , Element "ProductGroup" [] [ Text "eBooks" ]
                    , Element "Title"
                        []
                        [ Text "うちのクラスの女子がヤバい\x3000分冊版（１２）\x3000「リュウとランタンの灯り」 少年マガジンエッジコミックス" ]
                    ]
                ]
            ]
        ]
