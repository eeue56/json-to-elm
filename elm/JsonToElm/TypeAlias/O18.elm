module TypeAlias.O18 exposing (..)

import Regex exposing (regex, replace)
import Types
import TypeAlias exposing (TypeAlias, Field, capitalize, getFields, getTypeAliasName, getFieldNameAndType, prefixers, knownDecoders)


formatDecoderField : Field -> String
formatDecoderField field =
    let
        decoder =
            Types.knownTypesToString field.typeName
                |> String.split " "
                |> List.map TypeAlias.guessDecoder
                |> String.join " "
    in
        "(field \"" ++ field.name ++ "\" " ++ decoder ++ ")"


formatPipelineDecoderField : Field -> String
formatPipelineDecoderField field =
    let
        _ =
            Debug.log "field" field

        decoder =
            Types.knownTypesToString field.typeName
                |> String.split " "
                |> List.map TypeAlias.guessDecoder
                |> String.join " "
    in
        "|> Json.Decode.Pipeline.required \"" ++ field.name ++ "\" (" ++ decoder ++ ")"


formatEncoderField : Field -> String
formatEncoderField field =
    let
        encoder =
            Types.knownTypesToString field.typeName
                |> String.split " "
                |> List.map TypeAlias.guessEncoder
                |> List.map
                    (\s ->
                        if s == "Json.Encode.list" then
                            s ++ " <| List.map "
                        else
                            s ++ " <| "
                    )
                |> String.join ""
    in
        "(\"" ++ field.name ++ "\",  " ++ encoder ++ "record." ++ field.name ++ ")"


createDecoder : String -> String
createDecoder string =
    let
        withoutNewlines =
            replace Regex.All (regex "\\n") (\_ -> "") string

        typeName =
            getTypeAliasName withoutNewlines
                |> Maybe.withDefault ""
                |> capitalize

        parsedFields =
            getFields withoutNewlines

        fields =
            parsedFields
                |> List.map getFieldNameAndType
                |> List.map formatDecoderField
                |> String.join "\n        "

        numberOfFields =
            List.length parsedFields

        mapName =
            if numberOfFields == 1 then
                "map"
            else
                "map" ++ (toString numberOfFields)
    in
        if numberOfFields >= 7 then
            "import Json.Decode.Pipeline\n\n" ++ (createPipelineDecoder string)
        else
            String.join ""
                [ "decode"
                , typeName
                , " : Json.Decode.Decoder "
                , typeName
                , "\ndecode"
                , typeName
                , " =\n    Json.Decode."
                , mapName
                , " "
                , typeName
                , "\n        "
                , fields
                ]


createPipelineDecoder : String -> String
createPipelineDecoder string =
    let
        withoutNewlines =
            replace Regex.All (regex "\\n") (\_ -> "") string

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
            replace Regex.All (regex "\\n") (\_ -> "") string

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
    ""


runtimeCreateEncoder : TypeAlias -> String
runtimeCreateEncoder alias =
    ""


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
            , TypeAlias.runtimeObject alias.fields
            , "}; }"
            , functionWrapperEnd
            , ";"
            ]


originalImports : String
originalImports =
    """
import Json.Encode
import Json.Decode exposing (field)
    """
        |> String.trim


pipelineImports : String
pipelineImports =
    """
import Json.Encode
import Json.Decode
import Json.Decode.Pipeline
    """
        |> String.trim
