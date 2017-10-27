module XmlTest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import Xml
import XmlParser exposing (Node(..))


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
                    |> List.map (flip Xml.dig exampleXml)
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
                    |> List.map (flip Xml.q exampleXml)
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
        ]


exampleXml : XmlParser.Xml
exampleXml =
    { processingInstructions = []
    , docType = Nothing
    , root = exampleXmlElement
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
