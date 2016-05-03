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
    , value : Json.Value
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

camelCase : String -> String
camelCase name =
    case String.toList name of
        [] ->
            ""
        x::xs ->
            (String.toLower (String.fromChar x)) ++ (String.fromList xs)

fullyQualifiedName : Field -> String
fullyQualifiedName field =
    field.base ++ (capitalize field.name)

fieldFormat : Field -> String
fieldFormat field =
    case field.typeName of
        ComplexType ->
            (camelCase field.name) ++ " : " ++ (fullyQualifiedName field)
        _ ->
            (camelCase field.name) ++ " : " ++ (Types.knownTypesToString field.typeName)

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
        , value = field.value
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
            { name = String.trim x
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
    let
        _ =
            Debug.log "field" field

        decoder =
            Types.knownTypesToString field.typeName
                |> String.split " "
                |> List.map guessDecoder
                |> String.join " "
    in
        "|: (\"" ++ field.name ++ "\" := " ++ decoder ++ ")"


formatPipelineDecoderField : Field -> String
formatPipelineDecoderField field =
    let
        _ =
            Debug.log "field" field
        decoder =
            Types.knownTypesToString field.typeName
                |> String.split " "
                |> List.map guessDecoder
                |> String.join " "
    in
        "|> Json.Decode.Pipeline.required \"" ++ field.name ++ "\" (" ++ decoder ++ ")"


formatEncoderField : Field -> String
formatEncoderField field =
    let
        encoder =
            Types.knownTypesToString field.typeName
                |> String.split " "
                |> List.map guessEncoder
                |> List.map (\s ->
                    if s == "Json.Encode.list" then
                        s ++ " <| List.map "
                    else
                        s ++ " <| "
                )
                |> String.join ""
    in
        "(\"" ++ field.name ++ "\",  " ++ encoder ++ "record." ++ field.name ++  ")"

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
            , " : Json.Decode.Decoder "
            , typeName
            , "\ndecode"
            , typeName
            , " =\n    Json.Decode.succeed "
            , typeName
            , "\n        "
            , fields
            ]

createPipelineDecoder : String -> String
createPipelineDecoder string =
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
                |> List.map formatPipelineDecoderField
                |> String.join "\n        "

    in
        String.join ""
            [ "decode"
            , typeName
            , " : Json.Decode.Decoder "
            , typeName
            , "\ndecode"
            , typeName
            , " =\n    Json.Decode.Pipeline.decode "
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
            , " record =\n    Json.Encode.object\n        [ "
            , fields
            , "\n        ]"
            ]



{-
   var encodeBanana = function (record) {    return $Json$Encode.object(_U.list([{ctor: "_Tuple2",_0: "name",_1: $Json$Encode.string(record.name)}]));};
   var Banana = function (a) {    return {name: a};};
   var decodeBanana = A2($Json$Decode$Extra._op["|:"],$Json$Decode.succeed(Banana),A2($Json$Decode._op[":="],"name",$Json$Decode.string));

-}

runtimeObject : List Field -> String
runtimeObject fields =
    List.map .name fields
        |> List.map (\name -> name ++ " : " ++ name)
        |> String.join ",\n"

runtimeCreateConstructor : TypeAlias -> String
runtimeCreateConstructor alias =
    let
        functionWrapper =
            if List.length alias.fields < 2 then
                ""
            else
                "F" ++ (toString <| List.length alias.fields) ++ "("

        functionWrapperEnd =
            if functionWrapper == "" then
                ""
            else
                ")"
    in
        String.join ""
            [ "var "
            , alias.name
            , " = "
            , functionWrapper
            , "function ("
            , String.join ", " (List.map .name alias.fields)
            , ") {    return {"
            , runtimeObject alias.fields
            , "}; }"
            , functionWrapperEnd
            , ";"
            ]

prefixers =
    [ "int"
    , "float"
    ]

runtimeGuessDecoder : Field -> String
runtimeGuessDecoder field =
    let
        typeNames =
            Types.knownTypesToString field.typeName
                |> String.split " "

        closer =
            (List.repeat ((List.length typeNames) - 1) ")")
                |> String.join ""

        guessSingleDecoder typeName =
            let
                lower =
                    String.toLower typeName
            in
                if List.member lower knownDecoders then
                    if List.member lower prefixers then
                        "$Json$Decode.$" ++ lower
                    else
                        "$Json$Decode." ++ lower
                else
                    "decode" ++ (capitalize typeName)
    in
        typeNames
            |> List.map guessSingleDecoder
            |> String.join "("
            |> (\s -> s ++ closer)


runtimeGuessEncoder : Field -> String
runtimeGuessEncoder field =
    let
        typeName =
            Types.knownTypesToString field.typeName
        lower =
            String.toLower typeName
    in
        if List.member lower knownDecoders then
            if List.member lower prefixers then
                "$Json$Encode.$" ++ lower
            else
                "$Json$Encode." ++ lower
        else
            "decode" ++ (capitalize typeName)

runtimeDecodeField : Field -> String
runtimeDecodeField field =
    String.join ""
        [ "A2($Json$Decode._op[\":=\"],\""
        , field.name
        , "\","
        , runtimeGuessDecoder field
        , ")"
        ]

runtimeEncodeField : Field -> String
runtimeEncodeField field =
    String.join ""
        [ "{ctor: \"_Tuple2\",_0: \""
        , field.name
        , "\",_1: "
        , runtimeGuessEncoder field
        , "(record."
        , field.name
        , ")}"
        ]

runtimeCreateDecoder : TypeAlias -> String
runtimeCreateDecoder alias =
    let
        preamble =
            List.repeat (List.length alias.fields) "A2($Json$Decode$Extra._op[\"|:\"]"
                |> String.join ","

        ender =
            List.repeat (List.length alias.fields) ")"
                |> String.join ""
    in
        String.join ""
            [ "var decode"
            , alias.name
            , " = "
            , preamble
            , ",$Json$Decode.succeed("
            , alias.name
            , "),"
            , String.join ")," <| List.map runtimeDecodeField alias.fields
            , ");"
            ]
--var encodeBanana = function (record) {    return $Json$Encode.object(_U.list([{ctor: "_Tuple2",_0: "name",_1: $Json$Encode.string(record.name)}]));};

runtimeCreateEncoder : TypeAlias -> String
runtimeCreateEncoder alias =
    String.join ""
        [ "var encode"
        , alias.name
        , " = function (record) {    return $Json$Encode.object(_U.list(["
        , String.join "," <| List.map runtimeEncodeField alias.fields
        , "]));};"
        ]

isUnionType : String -> Bool
isUnionType input =
    if String.startsWith "type" input then
        String.startsWith "type alias" input
            |> not
    else
        False

isTypeAlias : String -> Bool
isTypeAlias input =
    String.startsWith "type alias" input

isJsonBlob : String -> Bool
isJsonBlob input =
    String.startsWith "{" input && String.endsWith "}" input

isDecoder : String -> Bool
isDecoder input =
    String.startsWith "decode" input


typeFromDecoder : String -> KnownTypes
typeFromDecoder input =
    case String.lines (Debug.log "in" input) of
        [] ->
            Unknown
        x::xs ->
            if String.contains "Json.Decode.Decoder" x then
                case String.split "Json.Decode.Decoder" x of
                    a::name::_ ->
                        ResolvedType (String.trim name)
                    _ ->
                        Unknown
            else if List.length xs == 0 then
                Unknown
            else
                typeFromDecoder (String.join "\n" xs)


guessTypeFromDecoder : String -> KnownTypes
guessTypeFromDecoder decoder =
    String.words decoder
        |> List.map String.trim
        |> List.map (\x ->
            if String.startsWith "Json.Decode." x then
                String.dropLeft 12 x
                    |> capitalize
            else
                capitalize x
            )
        |> String.join " "
        |> Types.typeToKnownTypes


fieldsFromDecoder : String -> List Field
fieldsFromDecoder input =
    String.lines input
        |> List.map String.trim
        |> List.filter (String.startsWith "|:")
        |> List.map (String.dropLeft 4)
        |> List.map (String.dropRight 1)
        |> List.filterMap (\line ->
            case String.split ":=" line of
                name::decoder::_ ->
                    Just
                        { name =
                            String.filter ((/=) '\"') name
                        , typeName =
                            guessTypeFromDecoder decoder
                        , base =
                            ""
                        , value =
                            Json.string line
                        }
                _ -> Nothing
            )

typeAliasFromDecoder : String -> TypeAlias
typeAliasFromDecoder input =
    let
        pieces =
            typeFromDecoder (String.trim input)
    in
        { name =
            Types.knownTypesToString pieces

        , fields =
            fieldsFromDecoder (String.trim input)
        , base =
            ""
        , typeName =
            pieces
        , value =
            Json.string input
        }


originalImports : String
originalImports =
    """
import Json.Encode
import Json.Decode exposing ((:=))
-- elm-package install --yes circuithub/elm-json-extra
import Json.Decode.Extra exposing ((|:))
    """
        |> String.trim


pipelineImports : String
pipelineImports =
    """
import Json.Encode
import Json.Decode
-- elm-package install --yes NoRedInk/elm-decode-pipeline
import Json.Decode.Pipeline
    """
        |> String.trim
