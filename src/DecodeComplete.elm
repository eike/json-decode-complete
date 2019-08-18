module DecodeComplete exposing (ObjectDecoder, object, complete, required, optional, discard, discardOptional, hardcoded, discardRest, rest, restValues, andThen, fail)

{-| This module provides a way to decode JSON objects while making sure that all fields are handled. The interface works similar to json-decode-pipeline. For example,

    import Json.Decode exposing (Decoder)
    import DecodeComplete exposing (..)

    type alias User =
        { name : String
        , age : Int
        }

    userDecoder : Decoder User
    userDecoder =
        object User
            |> require "name" Decode.string
            |> require "age" Decode.int
            |> discard "email"
            |> complete

decodes JSON objects that have precisely the fields `name`, `age`, and `email` and turns it into a `User` record, discarding the email address.

The general usage is as follows: Start decoding the object with `object f`, where `f` is the function being called with the results. Then decode the individual fields with `require`, `discard`, `optional`, `discardOptional`. At the end, turn turn the `ObjectDecoder` into a normal `Decoder` by calling `complete` (or `discardRest` or `rest` or `restValues`).

# Starting to decode
@docs ObjectDecoder, object

# Decoding fields
@docs required, optional, discard, discardOptional, hardcoded

# Finish decoding
@docs complete, discardRest, rest, restValues

# Special needs – decoding custom types and versioned data
@docs andThen, fail
-}

import Dict exposing (Dict)
import Json.Decode as D exposing (Decoder)
import Json.Encode as E


{-| A decoder for JSON objects that makes sure that all fields in the JSON are handled
-}
type ObjectDecoder a
    = OD (Decoder ( Dict String D.Value, a ))


{-| Start decoding a JSON object.
-}
object : a -> ObjectDecoder a
object a =
    OD (D.map2 Tuple.pair (D.dict D.value) (D.succeed a))


toOD : (( Dict String D.Value, a ) -> Decoder ( Dict String D.Value, b )) -> ObjectDecoder a -> ObjectDecoder b
toOD handler (OD objectDecoder) =
    OD (objectDecoder |> D.andThen handler)


{-| Decode the field given by the `String` parameter using the given (regular) `Decoder`. If the field is missing, the decoder fails.
-}
required : String -> Decoder a -> ObjectDecoder (a -> b) -> ObjectDecoder b
required field decoder =
    toOD
        (\( dict, f ) ->
            case Dict.get field dict of
                Just value ->
                    case D.decodeValue decoder value of
                        Ok a ->
                            D.succeed ( Dict.remove field dict, f a )

                        Err err ->
                            D.fail (D.errorToString err)

                Nothing ->
                    D.fail ("missing required field `" ++ field ++ "`")
        )


{-| Decode the field given by the `String` parameter using the given (regular) `Decoder`. If the field is missing, use the provided default value instead. However, if the `Decoder` fails, this `ObjectDecoder` fails as well.
-}
optional : String -> Decoder a -> a -> ObjectDecoder (a -> b) -> ObjectDecoder b
optional field decoder default =
    toOD
        (\( dict, f ) ->
            case Dict.get field dict of
                Just value ->
                    case D.decodeValue decoder value of
                        Ok a ->
                            D.succeed ( Dict.remove field dict, f a )

                        Err err ->
                            D.fail (D.errorToString err)

                Nothing ->
                    D.succeed ( dict, f default )
        )


{-| Require that a field is present, but discard its value.
-}
discard : String -> ObjectDecoder a -> ObjectDecoder a
discard field =
    toOD
        (\( dict, a ) ->
            if Dict.member field dict then
                D.succeed ( Dict.remove field dict, a )
            else
                D.fail ("missing required discarded field `" ++ field ++ "`")
        )


{-| Discard the value of a field (thus marking it as handled), but simply ignore if its not there.
-}
discardOptional : String -> ObjectDecoder a -> ObjectDecoder a
discardOptional field =
    toOD (\( dict, a ) -> D.succeed ( Dict.remove field dict, a ))


{-| Don’t look at the JSON, simply use the given value.
-}
hardcoded : a -> ObjectDecoder (a -> b) -> ObjectDecoder b
hardcoded a =
    toOD (\( dict, f ) -> D.succeed ( dict, f a ))


{-| Finish up the `ObjectDecoder`, turning it into a regular decoder. Pass a dictionary of the unhandled fields (as `Decode.Value` values).
-}
restValues : ObjectDecoder (Dict String D.Value -> b) -> Decoder b
restValues (OD objectDecoder) =
    (objectDecoder |> D.andThen (\( dict, f ) -> D.succeed (f dict)))


{-| Decode the remaining fields uniformly with the given `Decoder`, pass the dictionary of the results and close the `ObjectDecoder` turning it into a regular `Decoder`.
-}
rest : Decoder a -> ObjectDecoder (Dict String a -> b) -> Decoder b
rest aDecoder (OD objectDecoder) =
    (objectDecoder
        |> D.andThen
            (\( dict, f ) ->
                Dict.foldl
                    (\field value acc ->
                        case D.decodeValue aDecoder value of
                            Ok a ->
                                acc |> D.andThen (\newDict -> D.succeed (Dict.insert field a newDict))

                            Err err ->
                                D.fail (D.errorToString err)
                    )
                    (D.succeed Dict.empty)
                    dict
                    |> D.map f
            )
    )


{-| Decide how to proceed based on earlier fields. This can be useful if the JSON represents a sum type or has different versions. For example

    userDecoder : Decoder ( String, Int )
    userDecoder =
        (object identity
            |> required "version" D.int
        )
            |> andThen
                (\version ->
                    case version of
                        0 ->
                            object Tuple.pair
                                |> required "name" D.string
                                |> required "age" D.int

                        1 ->
                            object Tuple.pair
                                |> required "fullName" D.string
                                |> required "age" D.int
                                |> discard "email"

                        _ ->
                            fail "unsupported version"
                )
            |> complete

first decodes the `version` field. If it is `0`, the JSON needs to have (exactly) the fields `name` and `age`. If the version is `1`, the JSON needs the fields `fullName`, `age` and `email` instead. If the version is anything else, fail.
-}
andThen : (a -> ObjectDecoder b) -> ObjectDecoder a -> ObjectDecoder b
andThen cont =
    toOD
        (\( dict, a ) ->
            case cont a of
                OD subDecoder ->
                    case D.decodeValue subDecoder (E.dict identity identity dict) of
                        Ok result ->
                            D.succeed result

                        Err err ->
                            D.fail (D.errorToString err)
        )


{-| Turn the `ObjectDecoder` into a regular `Decoder`. Ignore if fields remain unhandled.

This might be useful if you only want the check that all fields are handled to occur during development. You can use `complete` in development and change it into `discardRest` without having to change anything else.
-}
discardRest : ObjectDecoder a -> Decoder a
discardRest (OD objectDecoder) =
    objectDecoder |> D.map Tuple.second


{-| Close the `ObjectDecoder`, turning it into a regular `Decoder`. If unhandled fields in the JSON remain, this decoder will fail.
-}
complete : ObjectDecoder a -> Decoder a
complete (OD objectDecoder) =
    objectDecoder
        |> D.andThen
            (\( dict, a ) ->
                if Dict.isEmpty dict then
                    D.succeed a
                else
                    D.fail ("The following fields where not handled: " ++ String.join ", " (Dict.keys dict))
            )


{-| An `ObjectDecoder` that always fails. Can be useful in combination with `andThen`.
-}
fail : String -> ObjectDecoder a
fail message =
    OD (D.fail message)
