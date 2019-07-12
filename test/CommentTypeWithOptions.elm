module CommentTypeWithOptions exposing (Comment)

import Dict exposing (Dict)
import Time


type alias Comment =
    { commentPostId : Int
    , commentText : String
    , commentMainCategories : (String, String)
    , commentPublished : Bool
    , commentCreated : Date
    , commentTags : Dict (String) (Int)
    }
