module TypeAlias where

import String
import Regex exposing (..)
import Types exposing (KnownTypes(..))
import Json.Encode as Json

knownDecoders : List String
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
    , fields : List Field
    , base : String
    , typeName : KnownTypes
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
    field.base ++ (capitalize field.name)

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
            List.map fieldFormat alias.fields
                |> String.join "\n    , "
    in
        String.join ""
            [ "type alias "
            , capitalize <| Types.knownTypesToString alias.typeName
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

        update : Field -> List Field -> List Field
        update field fine =
            if count field.name >= 1 then
                { field | typeName = ResolvedType <| fullyQualifiedName field } :: fine
            else
                { field | typeName = ResolvedType <| field.name } :: fine
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
                |> List.filter (\item -> item.base == fullyQualifiedName field)
    in
        { name = field.name
        , fields = currentFields
        , base = field.base
        , typeName = field.typeName
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
            generateFields stuff aliasName
                |> (::) topLevel

        creator field aliases =
            let
                knownNames =
                    List.map .name aliases

                alias =
                    createTypeAlias knownNames fields field
            in
                alias :: aliases

        aliases =
           gatherAliases fields
                |> resolveConflicts
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
            { name = ""
            , base = ""
            , typeName = Unknown
            , value = Json.string ""
            }
        [x] ->
            { name = x
            , base = ""
            , typeName = Unknown
            , value = Json.string ""
            }
        x::y::xs ->
            { name = String.trim <| String.toLower x
            , base = ""
            , typeName = ResolvedType y
            , value = Json.string ""
            }

guessDecoder : String -> String
guessDecoder typeName =
    if List.member (String.toLower typeName) knownDecoders then
        "Json.Decode." ++ (String.toLower typeName)
    else
        "decode" ++ (capitalize typeName)

guessEncoder : String -> String
guessEncoder typeName =
    if List.member (String.toLower typeName) knownDecoders then
        "Json.Encode." ++ (String.toLower typeName)
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
