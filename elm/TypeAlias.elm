module TypeAlias where

import String
import Set
import Regex exposing (..)
import Types
import Json.Encode as Json

knownDecoders =
    [ "maybe"
    , "list"
    , "int"
    , "float"
    , "bool"
    , "string"
    ]

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
            |> Set.fromList
            |> Set.toList




getTypeAliasName : String -> Maybe String
getTypeAliasName string =
    let
        pattern =
            regex "type alias(.+)\\="
    in
        case find (All) pattern string of
            [] -> Nothing
            [x] ->
                Just <| String.trim <| String.join "" <| List.map (Maybe.withDefault "") x.submatches
            _ ->
                Debug.log "too much" Nothing

getFields : String -> List String
getFields string =
    let
        withoutNewlines =
            replace All (regex "\\n") (\_ -> "") string
        pattern =
            regex "\\{(.+)\\}"
    in
        case find (All) pattern withoutNewlines of
            [] ->
                Debug.log "no matches" []
            [x] ->
                List.map (Maybe.withDefault "") x.submatches
                    |> String.join ""
                    |> String.split ","
                    |> List.map String.trim
            xs ->
                Debug.log ("too many matches") []


getFieldNameAndType : String -> (String, String)
getFieldNameAndType string =
    case String.split ":" string of
        [] -> ("", "")
        [x] -> (x, "")
        x::y::xs -> (String.trim x, String.trim y)

guessDecoder : String -> String
guessDecoder typeName =
    if List.member typeName knownDecoders then
        typeName
    else
        "decode" ++ (capitalize typeName)

guessEncoder : String -> String
guessEncoder typeName =
    if List.member typeName knownDecoders then
        typeName
    else
        "encode" ++ (capitalize typeName)


formatDecoderField : (String, String) -> String
formatDecoderField (key, value) =
    "|: (" ++ key ++ " := " ++ ( guessDecoder <| String.toLower value) ++ ")"


formatEncoderField : (String, String) -> String
formatEncoderField (key, value) =
    "(\"" ++ key ++ "\",  " ++ ( String.join " <| "<| String.split " " <| guessEncoder <| String.toLower value) ++ " record." ++ key ++  ")"

createDecoder : String -> String
createDecoder string =
    let
        withoutNewlines =
            replace All (regex "\\n") (\_ -> "") string
                |> Debug.log "n"
        typeName =
            getTypeAliasName withoutNewlines
                |> Debug.log "alias"
                |> Maybe.withDefault ""

        fields =
            getFields withoutNewlines
                |> List.map getFieldNameAndType
                |> List.map formatDecoderField
                |> String.join "\n        "

    in
        String.join ""
            [ "decode"
            , typeName
            , " : Decoder "
            , typeName
            , "\ndecode"
            , typeName
            , " =\n succeed "
            , typeName
            , "\n        "
            , fields
            ]

createEncoder : String -> String
createEncoder string =
    let
        withoutNewlines =
            replace All (regex "\\n") (\_ -> "") string
                |> Debug.log "n"
        typeName =
            getTypeAliasName withoutNewlines
                |> Debug.log "alias"
                |> Maybe.withDefault ""

        fields =
            getFields withoutNewlines
                |> List.map getFieldNameAndType
                |> List.map formatEncoderField
                |> String.join "\n        , "

    in
        String.join ""
            [ "encode"
            , typeName
            , " : "
            , typeName
            , " -> Json.Encode.Value"
            , "\nencode"
            , typeName
            , " record =\n     object\n        [ "
            , fields
            , "\n        ]"
            ]
