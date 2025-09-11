module Tests exposing (..)

import DecodeComplete
import Expect
import Json.Decode
import Test exposing (Test)


optionalTest : Test
optionalTest =
    Test.describe "optional fields"
        [ check "{}" (DecodeComplete.optional "a" Json.Decode.int -1) (Ok -1)
        , check "{ \"a\" : 2 }" (DecodeComplete.optional "a" Json.Decode.int -1) (Ok 2)
        , check "{ \"a\" : null }" (DecodeComplete.optional "a" Json.Decode.int -1) (Ok -1)
        , check "{ \"a\" : 2.3 }" (DecodeComplete.optional "a" Json.Decode.int -1) (Ok -1)
        ]


omissibleTest : Test
omissibleTest =
    Test.describe "omissible fields"
        [ check "{}" (DecodeComplete.omissible "a" Json.Decode.int -1) (Ok -1)
        , check "{ \"a\" : 2 }" (DecodeComplete.omissible "a" Json.Decode.int -1) (Ok 2)
        , check "{ \"a\" : null }" (DecodeComplete.omissible "a" Json.Decode.int -1) (Err ())
        , check "{ \"a\" : 2.3 }" (DecodeComplete.omissible "a" Json.Decode.int -1) (Err ())
        ]


check :
    String
    -> (DecodeComplete.ObjectDecoder (a -> a) -> DecodeComplete.ObjectDecoder a)
    -> Result e a
    -> Test
check input fieldDecoder result =
    Test.test input <|
        \_ ->
            let
                decoder : Json.Decode.Decoder a
                decoder =
                    DecodeComplete.object identity
                        |> fieldDecoder
                        |> DecodeComplete.complete
            in
            case result of
                Err _ ->
                    Json.Decode.decodeString decoder input
                        |> Expect.err

                Ok v ->
                    Json.Decode.decodeString decoder input
                        |> Expect.equal (Ok v)
