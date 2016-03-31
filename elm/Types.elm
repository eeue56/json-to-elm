module Types where

import Json.Encode as Json
import Json.Decode exposing ((:=))
import Json.Decode.Extra exposing ((|:))
import String
import Native.Types

type KnownTypes
    = MaybeType KnownTypes
    | ListType KnownTypes
    | IntType
    | FloatType
    | BoolType
    | StringType
    | ComplexType
    | ResolvedType String
    | Unknown


typeToKnownTypes : String -> KnownTypes
typeToKnownTypes string =
    case string of
        "Int" ->
            IntType
        "Float" ->
            FloatType
        "Bool" ->
            BoolType
        "String" ->
            StringType
        "Something" ->
            ComplexType
        _ ->
            case String.words string of
                [] ->
                    Unknown
                [x] ->
                    typeToKnownTypes "Something"
                x::xs ->
                    case x of
                        "Maybe" ->
                            MaybeType (typeToKnownTypes <| String.join " " xs)
                        "List" ->
                            ListType (typeToKnownTypes <| String.join " " xs)
                        _ ->
                            Unknown

knownTypesToString : KnownTypes -> String
knownTypesToString known =
    case known of
        Unknown ->
            "_Unknown"

        ComplexType ->
            "ComplexType"

        ResolvedType name ->
            String.trim name

        IntType ->
            "Int"

        FloatType ->
            "Float"

        StringType ->
            "String"

        BoolType ->
            "Bool"

        ListType nested ->
            "List " ++ (knownTypesToString nested)

        MaybeType nested ->
            "Maybe " ++ (knownTypesToString nested)



suggestType : Json.Value -> KnownTypes
suggestType value =
    Native.Types.makeGuessAtType value
        |> typeToKnownTypes


toValue : String -> Json.Value
toValue =
    Native.Types.toValue

keys : Json.Value -> List String
keys =
    Native.Types.keys

get : String -> Json.Value -> Maybe Json.Value
get =
    Native.Types.get

unsafeGet : String -> Json.Value -> Json.Value
unsafeGet =
    Native.Types.unsafeGet

unsafeEval : String -> String -> String -> String -> Json.Value -> Result String Json.Value
unsafeEval aliasName constructorString decoderString encoderString testData =
    Native.Types.unsafeEval aliasName constructorString decoderString encoderString testData



