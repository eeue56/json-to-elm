module TypeAlias where

import String
import Set
import Dict exposing (Dict)
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

type alias TypeAlias =
    { name : String
    , fields : Dict String String
    , base : String
    }

type alias Field =
    (String, String)

capitalize : String -> String
capitalize name =
    case String.toList name of
        [] ->
            ""
        x::xs ->
            (String.toUpper (String.fromChar x)) ++ (String.fromList xs)

fieldFormat : Field -> String
fieldFormat (key, typeName) =
    key ++ " : " ++ typeName


aliasFormat : TypeAlias -> String
aliasFormat alias =
    let
        joinedFields =
            Dict.toList alias.fields
                |> List.map fieldFormat
                |> String.join "\n    , "
    in

        String.join ""
            [ "type alias "
            , alias.name
            , " =\n    { "
            , joinedFields
            , "\n    }"
            ]

generateFields : Json.Value -> String -> List WithBase
generateFields stuff base =
    Types.keys stuff
        |> List.map (\key ->
            let
                value =
                    Types.unsafeGet key stuff
                name =
                    Types.suggestType value
            in
                if name == "Something" then
                    generateFields value (base ++ key) ++ [(base, key, name, value)]
                else
                    [(base, key, name, value)]
            )
        |> List.concat

type alias WithBase =
    (String, String, String, Json.Value)

type alias FieldWithoutBase =
    (String, String, Json.Value)

gatherAliases : List WithBase -> List WithBase
gatherAliases items =
    items
        |> List.filter (\(base, key, name, value) -> name == "Something")

resolveConflicts : List WithBase -> List WithBase
resolveConflicts items =
    let

        keys : List String
        keys =
            List.map (\(b, key, n, v) -> key) items

        count : String -> Int
        count incomingKey =
            List.filter (\key -> key == incomingKey) keys
                |> List.length

        update : WithBase -> List WithBase -> List WithBase
        update (base, key, name, value) fine =
            if count key > 1 then
                (base, key, capitalize base ++ capitalize key, value) :: fine
            else
                (base, key, capitalize key, value) :: fine
    in
        List.foldl update [] items

{-|

Looks like
{ Foo : TypeAlias
, FooFoo : TypeAlias
}

-}
createTypeAlias : Json.Value -> String -> String -> TypeAlias
createTypeAlias stuff aliasName base =
    let
        fields =
            generateFields stuff base
                |> List.map (\(_, key, name, _) -> (key, name))
                |> Dict.fromList
    in
        { name = aliasName
        , fields = fields
        , base = base
        }

createTypeAliases : Json.Value -> String -> String -> List TypeAlias
createTypeAliases stuff aliasName base =
    let
        fields =
            generateFields stuff base
            |> Debug.log "fields"
        aliases =
            gatherAliases fields
                |> Debug.log "gathered"
                |> resolveConflicts
                |> List.map (\(base, key, name, value) -> createTypeAlias value key base)
    in
        createTypeAlias stuff aliasName base :: aliases


isAlreadyAName : String -> List TypeAlias -> Bool
isAlreadyAName name aliases =
    List.member name (List.map .name aliases)


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


getFieldNameAndType : String -> Field
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


formatDecoderField : Field -> String
formatDecoderField (key, value) =
    "|: (" ++ key ++ " := " ++ ( guessDecoder <| String.toLower value) ++ ")"


formatEncoderField : Field -> String
formatEncoderField (key, value) =
    "(\"" ++ key ++ "\",  " ++ ( String.join " <| "<| String.split " " <| guessEncoder <| String.toLower value) ++ " record." ++ key ++  ")"

createDecoder : String -> String
createDecoder string =
    let
        withoutNewlines =
            replace All (regex "\\n") (\_ -> "") string
        typeName =
            getTypeAliasName withoutNewlines
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
        typeName =
            getTypeAliasName withoutNewlines
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
