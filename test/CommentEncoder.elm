module CommentEncoder exposing (..)

import CommentType exposing (..)
import Iso8601
import Json.Encode


encodeComment : Comment -> Json.Encode.Value
encodeComment x =
    Json.Encode.object
        [ ( "postId", Json.Encode.int x.postId )
        , ( "text", Json.Encode.string x.text )
        , ( "mainCategories", (\(m0, n0) -> Json.Encode.list identity [ Json.Encode.string m0, Json.Encode.string n0 ]) x.mainCategories )
        , ( "published", Json.Encode.bool x.published )
        , ( "created", (Json.Encode.string << Date.toIsoString) x.created )
        , ( "tags", (Json.Encode.dict Json.Encode.string Json.Encode.int) x.tags )
        ]
