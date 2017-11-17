module PAAPITest exposing (..)

import Expect exposing (..)
import Test exposing (..)
import PAAPI
import Util exposing ((=>))


suite : Test
suite =
    describe "PAAPI"
        -- Examples from:
        -- http://docs.aws.amazon.com/AWSECommerceService/latest/DG/rest-signature.html#rest_detailedexample
        [ test "`sign` should produce proper signature" <|
            \_ ->
                let
                    exampleRequests =
                        [ { locale = PAAPI.US
                          , params =
                                [ "Operation" => "ItemLookup"
                                , "ItemId" => "0679722769"
                                , "ResponseGroup" => "Images,ItemAttributes,Offers,Reviews"
                                , "Timestamp" => "2014-08-18T12:00:00Z"
                                ]
                          }
                        , { locale = PAAPI.UK
                          , params =
                                [ "Operation" => "ItemSearch"
                                , "Operation" => "ItemSearch" -- Duplicate!
                                , "Actor" => "Johnny Depp"
                                , "ResponseGroup" => "ItemAttributes,Offers,Images,Reviews,Variations"
                                , "SearchIndex" => "DVD"
                                , "Sort" => "salesrank"
                                , "Timestamp" => "2014-08-18T17:34:34.000Z" -- Milliseconds!
                                ]
                          }
                        ]

                    expectedSignatures =
                        [ "j7bZM0LXZ9eXeZruTqWm2DIvDYVUU3wxPPpp+iXxzQc="
                        , "Gv4kWyAAD3xgSGI86I4qZ1zIjAhZYs2H7CRTpeHLD1o="
                        ]
                in
                    exampleRequests
                        |> List.map (PAAPI.sign creds)
                        |> List.map (\( _, _, signature ) -> signature)
                        |> equalLists expectedSignatures
        , test "`signedUrl` should produce proper URL" <|
            \_ ->
                let
                    exampleRequests =
                        [ { locale = PAAPI.US
                          , params =
                                [ "Operation" => "ItemLookup"
                                , "ItemId" => "0679722769"
                                , "ResponseGroup" => "Images,ItemAttributes,Offers,Reviews"
                                , "Timestamp" => "2014-08-18T12:00:00Z"
                                ]
                          }
                        , { locale = PAAPI.UK
                          , params =
                                [ "Operation" => "ItemSearch"
                                , "Operation" => "ItemSearch" -- Duplicate!
                                , "Actor" => "Johnny Depp"
                                , "ResponseGroup" => "ItemAttributes,Offers,Images,Reviews,Variations"
                                , "SearchIndex" => "DVD"
                                , "Sort" => "salesrank"
                                , "Timestamp" => "2014-08-18T17:34:34.000Z" -- Milliseconds!
                                ]
                          }
                        , { locale = PAAPI.US
                          , params =
                                [ "Item.1.OfferListingId" => "j8ejq9wxDfSYWf2OCp6XQGDsVrWhl08GSQ9m5j+e8MS449BN1XGUC3DfU5Zw4nt/FBt87cspLow1QXzfvZpvzg=="
                                , "Item.1.Quantity" => "3"
                                , "Operation" => "CartCreate" -- For conflicting operations, one with sufficient parameters should prevail (presumably)
                                , "Operation" => "ItemSearch"
                                , "Timestamp" => "2014-08-18T17:36:55.000Z" -- Milliseconds!
                                ]
                          }
                        , { locale = PAAPI.US
                          , params =
                                [ "BrowseNodeId" => "465600"
                                , "Operation" => "BrowseNodeLookup" -- Should prevail
                                , "Operation" => "ItemSearch"
                                , "ResponseGroup" => "BrowseNodeInfo,TopSellers,NewReleases,MostWishedFor,MostGifted"
                                , "Timestamp" => "2014-08-18T17:38:12.000Z" -- Milliseconds!
                                ]
                          }
                        , { locale = PAAPI.US
                          , params =
                                [ "Condition" => "New"
                                , "ItemId" => "B0011ZK6PC,B000NK8EWI"
                                , "Merchant" => "Amazon"
                                , "Operation" => "ItemSearch"
                                , "Operation" => "SimilarityLookup" -- Should prevail
                                , "ResponseGroup" => "Offers,ItemAttributes"
                                , "SimilarityType" => "Intersection"
                                , "Timestamp" => "2014-08-18T17:39:22.000Z" -- Milliseconds!
                                ]
                          }
                        ]

                    expectedUrls =
                        [ "https://webservices.amazon.com/onca/xml?AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&AssociateTag=mytag-20&ItemId=0679722769&Operation=ItemLookup&ResponseGroup=Images%2CItemAttributes%2COffers%2CReviews&Service=AWSECommerceService&Timestamp=2014-08-18T12%3A00%3A00Z&Version=2013-08-01&Signature=j7bZM0LXZ9eXeZruTqWm2DIvDYVUU3wxPPpp%2BiXxzQc%3D"
                        , "https://webservices.amazon.co.uk/onca/xml?AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&Actor=Johnny%20Depp&AssociateTag=mytag-20&Operation=ItemSearch&Operation=ItemSearch&ResponseGroup=ItemAttributes%2COffers%2CImages%2CReviews%2CVariations&SearchIndex=DVD&Service=AWSECommerceService&Sort=salesrank&Timestamp=2014-08-18T17%3A34%3A34.000Z&Version=2013-08-01&Signature=Gv4kWyAAD3xgSGI86I4qZ1zIjAhZYs2H7CRTpeHLD1o%3D"
                        , "https://webservices.amazon.com/onca/xml?AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&AssociateTag=mytag-20&Item.1.OfferListingId=j8ejq9wxDfSYWf2OCp6XQGDsVrWhl08GSQ9m5j%2Be8MS449BN1XGUC3DfU5Zw4nt%2FFBt87cspLow1QXzfvZpvzg%3D%3D&Item.1.Quantity=3&Operation=CartCreate&Operation=ItemSearch&Service=AWSECommerceService&Timestamp=2014-08-18T17%3A36%3A55.000Z&Version=2013-08-01&Signature=LpEUnc9tT4WGneeUwH0LvwxLLfbMEXgmjGX5GXQ1MEQ%3D"
                        , "https://webservices.amazon.com/onca/xml?AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&AssociateTag=mytag-20&BrowseNodeId=465600&Operation=BrowseNodeLookup&Operation=ItemSearch&ResponseGroup=BrowseNodeInfo%2CTopSellers%2CNewReleases%2CMostWishedFor%2CMostGifted&Service=AWSECommerceService&Timestamp=2014-08-18T17%3A38%3A12.000Z&Version=2013-08-01&Signature=t48XyuQKLcYROCm7w%2FNqo3mihqB%2FQF2B9b9SX3FIOnU%3D"
                        , "https://webservices.amazon.com/onca/xml?AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE&AssociateTag=mytag-20&Condition=New&ItemId=B0011ZK6PC%2CB000NK8EWI&Merchant=Amazon&Operation=ItemSearch&Operation=SimilarityLookup&ResponseGroup=Offers%2CItemAttributes&Service=AWSECommerceService&SimilarityType=Intersection&Timestamp=2014-08-18T17%3A39%3A22.000Z&Version=2013-08-01&Signature=nIlF7C6O1T3faoXIZgGVxYXd%2BD%2F39%2BFPSnwdfiQvy9g%3D"
                        ]
                in
                    exampleRequests
                        |> List.map (PAAPI.signedUrl creds)
                        |> equalLists expectedUrls
        ]


creds : PAAPI.Credentials
creds =
    { accessKeyId = "AKIAIOSFODNN7EXAMPLE"
    , secretAccessKey = "1234567890"
    , associateTag = "mytag-20"
    }
