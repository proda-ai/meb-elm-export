module CommentType exposing (Comment)

import Dict exposing (Dict)
import Time


type alias Comment =
    { postId : Int
    , text : String
    , mainCategories : (String, String)
    , published : Bool
    , created : Date
    , tags : Dict (String) (Int)
    }
