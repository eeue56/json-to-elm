module UnionType where

import Types exposing (KnownTypes(..))
import Dict exposing (Dict)
import String
import List.Extra exposing (dropWhile)

type alias UnionType =
    { name : String
    , fields : Dict String KnownTypes
    }


getName : String -> String
getName blob =
    let
        equalIndex =
            String.indexes "=" blob
                |> List.head
                |> Maybe.withDefault -1
    in
        String.slice 4 equalIndex blob
            |> String.trim


getFields : String -> List String
getFields blob =
    String.toList blob
        |> dropWhile ((/=) '=')
        |> List.drop 1
        |> String.fromList
        |> String.split "|"
        |> List.map String.trim


createUnionType : String -> UnionType
createUnionType blob =
    { name =
        getName blob
    , fields =
        getFields blob
            |> List.map (\field -> (field, ResolvedType field))
            |> Dict.fromList
    }


formatConstructorDecoder : String -> String
formatConstructorDecoder field =
    String.join ""
        [ "\""
        , field
        , "\" -> Result.Ok "
        , field
        ]


toStringName : UnionType -> String
toStringName union =
    "toString" ++ union.name

fromStringName : UnionType -> String
fromStringName union =
    "fromString" ++ union.name

createTypeFromString : UnionType -> String
createTypeFromString union =
    let
        formattedConstructors =
            Dict.keys union.fields
                |> List.map formatConstructorDecoder
                |> String.join "\n        "
    in
        String.join ""
            [ fromStringName union
            , " : "
            , union.name
            , " -> Result String String"
            , "\n"
            , fromStringName union
            , " string = "
            , "\n    "
            , "case string of\n        "
            , formattedConstructors
            , "\n        _ -> Result.Err (\"Not valid pattern for decoder to "
            , union.name
            , ". Pattern: \" ++ (toString string))"
            ]



createDecoder : UnionType -> String
createDecoder union =
    String.join ""
        [ "decode"
        , union.name
        , " : Json.Decode.Decoder "
        , union.name
        , "\ndecode"
        , union.name
        , " =\n"
        , "    Json.Decode.string `Json.Decode.andThen` "
        , fromStringName union
        ]

createEncoder : UnionType -> String
createEncoder union =
    let
        formattedConstructors =
            Dict.keys union.fields
                |> List.map formatConstructorDecoder
                |> String.join "\n                "

    in
        String.join ""
            [ "encode"
            , union.name
            , " : "
            , union.name
            , " -> Json.Value"
            , "\nencode"
            , union.name
            , " =\n toString >> Json.Encode.string"
            ]
