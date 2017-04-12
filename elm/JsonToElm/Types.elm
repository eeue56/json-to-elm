module JsonToElm.Types exposing (..)

import Json.Encode as Json
import Json.Decode as Decode
import String


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


{-|
    >>> removeParens "(List String)"
    "List String"
-}
removeParens : String -> String
removeParens =
    String.trim
        >> String.split "("
        >> (\xs ->
                if List.length xs > 1 then
                    List.drop 1 xs
                else
                    xs
           )
        >> String.join "("
        >> String.split ")"
        >> (\xs ->
                if List.length xs > 1 then
                    List.take (List.length xs - 1) xs
                else
                    xs
           )
        >> String.join ")"


{-| Convert a type based on the name as a string into a representation of Elm
    >>> typeToKnownTypes "Int"
    IntType

    >>> typeToKnownTypes "Flip"
    ComplexType

    >>> typeToKnownTypes ""
    ComplexType

    >>> typeToKnownTypes "List String"
    ListType StringType

    >>> typeToKnownTypes "Maybe (List String)"
    MaybeType (ListType StringType)
-}
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

                [ x ] ->
                    typeToKnownTypes "Something"

                x :: xs ->
                    case x of
                        "Maybe" ->
                            String.join " " xs
                                |> removeParens
                                |> typeToKnownTypes
                                |> MaybeType

                        "List" ->
                            String.join " " xs
                                |> removeParens
                                |> typeToKnownTypes
                                |> ListType

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


knownTypesToEnglish : KnownTypes -> String
knownTypesToEnglish known =
    case known of
        Unknown ->
            "I don't know what this is"

        ComplexType ->
            "something that has to be written by hand!"

        ResolvedType name ->
            "a type that you've called " ++ (String.trim name)

        IntType ->
            "an int value"

        FloatType ->
            "a float value"

        StringType ->
            "a string value"

        BoolType ->
            "a boolean value"

        ListType nested ->
            "a list of " ++ (knownTypesToString nested)

        MaybeType nested ->
            "an optional value of " ++ (knownTypesToString nested)


suggestType : Json.Value -> KnownTypes
suggestType value =
    Native.Types.makeGuessAtType value
        |> typeToKnownTypes


keys : Json.Value -> List String
keys =
    Decode.decodeValue (Decode.keyValuePairs Decode.value)
        >> Result.withDefault []
        >> List.map Tuple.first


keysFromString : String -> List String
keysFromString =
    Decode.decodeString (Decode.keyValuePairs Decode.value)
        >> Result.withDefault []
        >> List.map Tuple.first


unsafeGet : String -> Json.Value -> Json.Value
unsafeGet key =
    Decode.decodeValue (Decode.field key Decode.value)
        >> Result.withDefault (Json.null)


unsafeGetFromString : String -> String -> Json.Value
unsafeGetFromString key =
    Decode.decodeString (Decode.field key Decode.value)
        >> Result.withDefault (Json.null)
