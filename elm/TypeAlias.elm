module TypeAlias exposing (..)

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


{-|
    >>> capitalize "hello"
    "Hello"
    >>> capitalize "Hello"
    "Hello"
    >>> capitalize "hello you"
    "Hello you"
    >>> capitalize ""
    ""
-}
capitalize : String -> String
capitalize name =
    case String.toList name of
        [] ->
            ""

        x :: xs ->
            (String.toUpper (String.fromChar x)) ++ (String.fromList xs)


badCharsRegex : Regex
badCharsRegex =
    regex "[_.\\- ]"


{-|
    >>> cleanString "hello-dog"
    "helloDog"

    >>> cleanString "hello_dog"
    "helloDog"

    >>> cleanString "hello dog"
    "helloDog"

    >>> cleanString "hello.dog"
    "helloDog"
-}
cleanString : String -> String
cleanString name =
    case split All badCharsRegex name of
        [] ->
            ""

        x :: xs ->
            x ++ (List.map capitalize xs |> String.join "")


{-|
    >>> camelCase "hello dog-cat"
    "helloDogCat"
-}
camelCase : String -> String
camelCase name =
    case String.toList name of
        [] ->
            ""

        x :: xs ->
            (String.toLower (String.fromChar x)) ++ (cleanString (String.fromList xs))


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
        |> List.map
            (\key ->
                let
                    value =
                        Types.unsafeGet key stuff

                    name =
                        Types.suggestType value

                    newBase =
                        base
                            ++ (capitalize key)
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
            [] ->
                Nothing

            [ x ] ->
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

            [ x ] ->
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

        [ x ] ->
            { name = x
            , base = ""
            , typeName = Unknown
            , value = Json.string ""
            }

        x :: y :: xs ->
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


runtimeObject : List Field -> String
runtimeObject fields =
    List.map .name fields
        |> List.map (\name -> name ++ " : " ++ name)
        |> String.join ",\n"


prefixers : List String
prefixers =
    [ "int"
    , "float"
    ]


{-|
    >>> isUnionType "type alias Model = {}"
    False

    >>> isUnionType "type Animal = Dog | Cat"
    True
-}
isUnionType : String -> Bool
isUnionType input =
    if String.startsWith "type" input then
        String.startsWith "type alias" input
            |> not
    else
        False


{-|
    >>> isTypeAlias "type alias Model = {}"
    True

    >>> isTypeAlias "hello"
    False
-}
isTypeAlias : String -> Bool
isTypeAlias input =
    String.startsWith "type alias" input


{-|
    >>> isJsonBlob "hello"
    False
    >>> isJsonBlob "{ hello: dog }"
    True
-}
isJsonBlob : String -> Bool
isJsonBlob input =
    String.startsWith "{" input && String.endsWith "}" input


isDecoder : String -> Bool
isDecoder input =
    String.startsWith "decode" input


typeFromDecoder : String -> KnownTypes
typeFromDecoder input =
    case String.lines input of
        [] ->
            Unknown

        x :: xs ->
            if String.contains "Json.Decode.Decoder" x then
                case String.split "Json.Decode.Decoder" x of
                    a :: name :: _ ->
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
        |> List.map
            (\x ->
                if String.startsWith "Json.Decode." x then
                    String.dropLeft 12 x
                        |> capitalize
                else
                    capitalize x
            )
        |> String.join " "
        |> Types.typeToKnownTypes


fieldsO17FromDecoder : String -> List Field
fieldsO17FromDecoder input =
    String.lines input
        |> List.map String.trim
        |> List.filter (String.startsWith "|:")
        |> List.map (String.dropLeft 4)
        |> List.map (String.dropRight 1)
        |> List.filterMap
            (\line ->
                case String.split ":=" line of
                    name :: decoder :: _ ->
                        Just
                            { name =
                                String.filter ((/=) '"') name
                            , typeName =
                                guessTypeFromDecoder decoder
                            , base =
                                ""
                            , value =
                                Json.string line
                            }

                    _ ->
                        Nothing
            )


fieldsO18FromDecoder : String -> List Field
fieldsO18FromDecoder input =
    String.lines input
        |> List.map String.trim
        |> List.filter (String.startsWith "(field")
        |> List.map (String.dropLeft 6)
        |> List.map (String.dropRight 1)
        |> List.map String.trim
        |> List.filterMap
            (\line ->
                case String.split " " line of
                    name :: decoder :: _ ->
                        Just
                            { name =
                                String.filter ((/=) '"') name
                            , typeName =
                                guessTypeFromDecoder decoder
                            , base =
                                ""
                            , value =
                                Json.string line
                            }

                    _ ->
                        Nothing
            )


fieldFromPipelineDecoder : String -> List Field
fieldFromPipelineDecoder input =
    String.lines input
        |> List.map String.trim
        |> List.filter (String.startsWith "|>")
        |> List.map (String.dropLeft 1)
        |> List.map String.trim
        |> List.filterMap
            (\line ->
                case String.split " " line of
                    _ :: req :: name :: decoder :: _ ->
                        Just
                            { name =
                                String.filter ((/=) '"') name
                            , typeName =
                                guessTypeFromDecoder (String.dropLeft 1 decoder |> String.dropRight 1)
                            , base =
                                ""
                            , value =
                                Json.string line
                            }

                    _ ->
                        Nothing
            )


typeAliasFromDecoder : String -> TypeAlias
typeAliasFromDecoder input =
    let
        pieces =
            typeFromDecoder (String.trim input)

        fields =
            if String.contains "|:" input then
                fieldsO17FromDecoder
            else if String.contains "Pipeline" input then
                fieldFromPipelineDecoder
            else
                fieldsO18FromDecoder
    in
        { name =
            Types.knownTypesToString pieces
        , fields =
            fields (String.trim input)
        , base =
            ""
        , typeName =
            pieces
        , value =
            Json.string input
        }


formatEnglishField : Field -> String
formatEnglishField field =
    "It must have a field called `" ++ field.name ++ "` with the type " ++ (Types.knownTypesToString field.typeName)


formatEnglishTypeAlias : TypeAlias -> String
formatEnglishTypeAlias alias =
    String.join "\n"
        [ "If successful, create a record of the type " ++ alias.name ++ ""
        , List.map formatEnglishField alias.fields |> String.join "\n"
        ]
