module TypeAlias where

import String
import Set
import Dict exposing (Dict)
import Regex exposing (..)
import Types exposing (KnownTypes(..))
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
    , fields : Dict String Field
    , base : String
    }

type alias Field =
    { name : String
    , typeName : KnownTypes
    , base : String
    , value : Json.Value
    }

capitalize : String -> String
capitalize name =
    case String.toList name of
        [] ->
            ""
        x::xs ->
            (String.toUpper (String.fromChar x)) ++ (String.fromList xs)

fullyQualifiedName : Field -> String
fullyQualifiedName field =
    let
        _ =
            Debug.log "field" field
    in
        field.base ++ (capitalize field.name)
            |> capitalize
            |> Debug.log "fully "

fieldFormat : Field -> String
fieldFormat field =
    case field.typeName of
        ComplexType ->
            field.name ++ " : " ++ (fullyQualifiedName field)
        _ ->
            field.name ++ " : " ++ (Types.knownTypesToString field.typeName)

aliasFormat : TypeAlias -> String
aliasFormat alias =
    let
        joinedFields =
            Dict.values alias.fields
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

generateFields : Json.Value -> String -> List Field
generateFields stuff base =
    Types.keys stuff
        |> List.map (\key ->
            let
                value =
                    Types.unsafeGet key stuff
                name =
                    Types.suggestType value
                        |> Debug.log "name: "
                newBase =
                    base ++ (capitalize key)
                        |> capitalize

                field =
                    { base = String.trim base
                    , name = String.trim key
                    , typeName = name
                    , value = value
                    }
            in
                if name == ComplexType then
                    generateFields value newBase ++ [ field ]
                else
                    [ field ]
            )
        |> List.concat


gatherAliases : List Field -> List Field
gatherAliases items =
    items
        |> List.filter (\item -> item.typeName == ComplexType)

resolveConflicts : List Field -> List Field
resolveConflicts items =
    let

        names : List String
        names =
            List.map .name items

        count : String -> Int
        count incomingKey =
            List.filter (\name -> name == incomingKey) names
                |> List.length
                |> Debug.log "count: "

        update : Field -> List Field -> List Field
        update field fine =
            if count field.name > 1 then
                { field | name = field.base ++ capitalize field.name } :: fine
            else
                { field | name = field.name } :: fine
    in
        List.foldl update [] items

{-|

Looks like
{ Foo : TypeAlias
, FooFoo : TypeAlias
}

-}
createTypeAlias : List String -> List Field -> Field -> TypeAlias
createTypeAlias knownNames fields field =
    let
        currentFields =
            fields
                |> List.filter (\item -> item.base == field.base)
                |> List.map (\field -> (field.name, field))
                |> Dict.fromList
    in
        { name =
            if List.member field.name knownNames then
                field.base ++ field.name
                    |> capitalize
            else
                capitalize field.name
        , fields = currentFields
        , base = field.base
        }

createTypeAliases : Json.Value -> String -> String -> List TypeAlias
createTypeAliases stuff aliasName base =
    let
        topLevel : Field
        topLevel =
            { name = aliasName
            , base = ""
            , typeName = ComplexType
            , value = stuff
            }


        fields =
            generateFields stuff (Debug.log "alais" aliasName)
                |> (::) topLevel
                |> Debug.log "fields"

        fieldBuilder : Field -> Dict String (List Field) -> Dict String (List Field)
        fieldBuilder field dict =
            let
                updater item =
                    case item of
                        Nothing ->
                            Just [ field ]
                        Just values ->
                            Just (field :: values)
            in
                Dict.update base updater dict


        fieldsByBase =
            fields
                |> List.foldl fieldBuilder Dict.empty
                |> Debug.log "by base: "

        creator field aliases =
            let
                knownNames =
                    List.map .name aliases
                        |> Debug.log "known: "

                alias =
                    createTypeAlias knownNames fields field
            in
                alias :: aliases
        aliases =
           gatherAliases fields
                |> Debug.log "before conflicts: "
                --|> resolveConflicts
                --|> (::) topLevel
                |> Debug.log "after conflicts: "
                |> List.foldl creator []
    in
        aliases


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
                    |> List.map String.trim
                    |> String.join ""
                    |> String.split ","
                    |> List.map String.trim
            xs ->
                Debug.log ("too many matches") []


getFieldNameAndType : String -> Field
getFieldNameAndType string =
    case String.split ":" string of
        [] ->
            { name = "", base = "", typeName = Unknown, value = Json.string ""}
        [x] ->
            { name = x, base = "", typeName = Unknown, value = Json.string ""}
        x::y::xs ->
            { name = String.trim <| String.toLower x , base = "", typeName = ResolvedType (Debug.log "y" y), value = Json.string ""}

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
formatDecoderField field =
    "|: (\"" ++ field.name ++ "\" := " ++ ( guessDecoder <| Types.knownTypesToString field.typeName ) ++ ")"


formatEncoderField : Field -> String
formatEncoderField field =
    "(\"" ++ field.name ++ "\",  " ++ ( String.join " <| "<| String.split " " <| guessEncoder <| Types.knownTypesToString field.typeName) ++ " record." ++ field.name ++  ")"

createDecoder : String -> String
createDecoder string =
    let
        withoutNewlines =
            replace All (regex "\\n") (\_ -> "") string

        typeName =
            getTypeAliasName withoutNewlines
                |> Maybe.withDefault ""
                |> capitalize

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
            , " =\n    succeed "
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
                |> capitalize

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
            , " record =\n    object\n        [ "
            , fields
            , "\n        ]"
            ]
