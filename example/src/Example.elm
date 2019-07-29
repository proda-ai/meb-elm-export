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

stringFromOnlyThree : OnlyThree -> String
stringFromOnlyThree x =
        One ->
            "One"

        Two ->
            "Two"

        Three ->
            "Three"

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

type OfficeRef
    = ExistingOffice Int
    | NewOffice Int

decodeOfficeRef : Decoder OfficeRef
decodeOfficeRef =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "ExistingOffice" ->
                        succeed ExistingOffice
                            |> required "contents" int

                    "NewOffice" ->
                        succeed NewOffice
                            |> required "contents" int

                    _ ->
                        fail "Constructor not matched"
            )

encodeOfficeRef : OfficeRef -> Json.Encode.Value
encodeOfficeRef x =
    case x of
        ExistingOffice y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "ExistingOffice" )
                , ( "contents", Json.Encode.int y0 )
                ]

        NewOffice y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "NewOffice" )
                , ( "contents", Json.Encode.int y0 )
                ]

type Pairs
    = FirstPair String
    | SecondPair String Int

decodePairs : Decoder Pairs
decodePairs =
    field "tag" string
        |> andThen
            (\x ->
                case x of
                    "FirstPair" ->
                        succeed FirstPair
                            |> required "contents" string

                    "SecondPair" ->
                        succeed SecondPair
                            |> required "contents" (index 0 string)
                            |> required "contents" (index 1 int)

                    _ ->
                        fail "Constructor not matched"
            )

encodePairs : Pairs -> Json.Encode.Value
encodePairs x =
    case x of
        FirstPair y0 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "FirstPair" )
                , ( "contents", Json.Encode.string y0 )
                ]

        SecondPair y0 y1 ->
            Json.Encode.object
                [ ( "tag", Json.Encode.string "SecondPair" )
                , ( "contents", Json.Encode.list identity [ Json.Encode.string y0, Json.Encode.int y1 ] )
                ]