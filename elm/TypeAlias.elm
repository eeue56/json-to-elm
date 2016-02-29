module TypeAlias where

import String
import Types
import Json.Encode as Json

capitalize : String -> String
capitalize name =
    case String.toList name of
        [] ->
            ""
        x::xs ->
            (String.toUpper (String.fromChar x)) ++ (String.fromList xs)

fieldFormat : (String, String) -> String
fieldFormat (key, typeName) =
    key ++ " : " ++ typeName


aliasFormat : String -> List (String, String) -> String
aliasFormat aliasName fields =
    let
        joinedFields =
            List.map fieldFormat fields
                |> String.join "\n    , "
    in

        String.join ""
            [ "type alias "
            , aliasName
            , " =\n    { "
            , joinedFields
            , "\n    }"
            ]


createTypeAlias : Json.Value -> String -> List String
createTypeAlias stuff aliasName =
    let
        fields : List (String, String, Json.Value)
        fields =
            Types.keys stuff
                |> List.map (\key ->
                    let
                        value =
                            Types.unsafeGet key stuff
                    in
                        (key, Types.suggestType value, value)
                    )

        fixedFields =
            fields
                |> List.map (\(key, typeName, _) ->
                    case typeName of
                        "Something" ->
                            (key, capitalize key)
                        _ ->
                            (key, typeName)
                    )

        update (key, typeName, value) extras =
            case typeName of
                "Something" ->
                    (createTypeAlias value (capitalize key)) ++ extras
                _ ->
                    extras

        extraAliases =
            List.foldl update [] fields
    in
        (aliasFormat aliasName fixedFields) :: extraAliases
