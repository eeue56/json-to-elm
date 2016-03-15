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


createDecoder : UnionType -> String
createDecoder union =
    let
        formattedConstructors =
            Dict.keys union.fields
                |> List.map formatConstructorDecoder
                |> String.join "\n                "

    in
        String.join ""
            [ "decode"
            , union.name
            , " : Json.Decode.Decoder "
            , union.name
            , "\ndecode"
            , union.name
            , " =\n    let\n       decodeToType string =\n"
            , "            case string of\n                "
            , formattedConstructors
            , "\n_ -> Result.Err (\"Not valid pattern for decoder to "
            , union.name
            , ". Pattern: \" ++ (toString string))\n    in"
            , "\n        customDecoder Json.Decode.string decodeToType"
            ]
