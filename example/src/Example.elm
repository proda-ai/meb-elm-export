module Example exposing (..)

import Date
import Date exposing (Date)
import Iso8601
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Time

type alias Person =
    { id : Int
    , name : Maybe (String)
    , birth : Date
    , accountCreation : Time.Posix
    }

decodePerson : Decoder Person
decodePerson =
    succeed Person
        |> required "id" int
        |> required "name" (nullable string)
        |> required "birth" (Json.Decode.map (Date.fromIsoString) string)
        |> required "accountCreation" Iso8601.decoder

encodePerson : Person -> Json.Encode.Value
encodePerson x =
    Json.Encode.object
        [ ( "id", Json.Encode.int x.id )
        , ( "name", (Maybe.withDefault Json.Encode.null << Maybe.map Json.Encode.string) x.name )
        , ( "birth", (Json.Encode.string << Date.toIsoString) x.birth )
        , ( "accountCreation", Iso8601.encode x.accountCreation )
        ]

type OnlyThree
    = One
    | Two
    | Three

decodeOnlyThree : Decoder OnlyThree
decodeOnlyThree =
    string
        |> andThen
            (\x ->
                case x of
                    "One" ->
                        succeed One

                    "Two" ->
                        succeed Two

                    "Three" ->
                        succeed Three

                    _ ->
                        fail "Constructor not matched"
            )

encodeOnlyThree : OnlyThree -> Json.Encode.Value
encodeOnlyThree x =
    case x of
        One ->
            Json.Encode.string "One"

        Two ->
            Json.Encode.string "Two"

        Three ->
            Json.Encode.string "Three"

stringFromOnlyThree : OnlyThree -> Maybe String
stringFromOnlyThree x =
        Just One ->
            Just "One"

        Just Two ->
            Just "Two"

        Just Three ->
            Just "Three"
        
        _ -> Nothing

stringFromMaybeOnlyThree : Maybe (OnlyThree) -> Maybe String
stringFromMaybeOnlyThree x =
    case x of 
        Just One ->
            Just "One"

        Just Two ->
            Just "Two"

        Just Three ->
            Just "Three"
        
        _ -> Nothing

stringToMaybeOnlyThree : String ->  Maybe (OnlyThree)
stringToMaybeOnlyThree x = 
    case x of 
        "One" ->
            Just One

        "Two" ->
            Just Two

        "Three" ->
            Just Three
        
        _ -> Nothing

stringToMaybeOnlyThree : String ->  Maybe (OnlyThree)
stringToMaybeOnlyThree x = 
    case x of 
        "One" ->
            Just One

        "Two" ->
            Just Two

        "Three" ->
            Just Three
        
        _ -> Nothing