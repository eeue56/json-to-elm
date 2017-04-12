module Home exposing (..)

import JsonToElm.TypeAlias as TypeAlias exposing (TypeAlias)
import JsonToElm.TypeAlias.O17 as TypeAliasO17
import JsonToElm.TypeAlias.O18 as TypeAliasO18
import JsonToElm.UnionType as UnionType
import JsonToElm.Types as Types
import String
import Util
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Css exposing (..)
import Css.Elements as Css
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import ColorScheme exposing (..)
import Json.Decode
import Json.Encode


cssNamespace : String
cssNamespace =
    "homepage"


{ class, classList, id } =
    Html.CssHelpers.withNamespace cssNamespace


type CssClasses
    = Content
    | Input
    | Output
    | Alias


css : Stylesheet
css =
    (stylesheet << namespace cssNamespace)
        [ Css.body
            [ backgroundColor accent1 ]
        , (.) Content
            [ Css.width (px 960)
            , margin2 zero auto
            ]
        , each [ (.) Input, (.) Output ]
            [ Css.width (pct 40)
            , Css.height (px 500)
            , fontFamily monospace
            ]
        , aliasCss
        ]


onChange : msg -> Attribute msg
onChange msg =
    Html.Events.on "change" (Json.Decode.succeed msg)


viewAlias : String -> Html Action
viewAlias alias =
    div [] [ Html.text alias ]


viewJson : String -> Html Action
viewJson json =
    div
        []
        [ Html.text <| "The entered json was: " ++ json ]


viewOutput : ElmVersion -> String -> Html Action
viewOutput version alias =
    let
        decoderText =
            case version of
                O17 ->
                    TypeAliasO17.createDecoder alias

                O18 ->
                    TypeAliasO18.createDecoder alias
    in
        node "pre"
            [ value decoderText
            , class [ Output ]
            ]
            [ Html.text <| decoderText ]


viewAllAliases : ElmVersion -> String -> DecoderType -> List TypeAlias -> Html Action
viewAllAliases version incoming decoder aliases =
    let
        formattedAliases =
            List.map TypeAlias.aliasFormat aliases

        decoderCreator =
            case decoder of
                Pipeline ->
                    case version of
                        O17 ->
                            TypeAliasO17.createPipelineDecoder

                        O18 ->
                            TypeAliasO18.createPipelineDecoder

                _ ->
                    case version of
                        O17 ->
                            TypeAliasO17.createDecoder

                        O18 ->
                            TypeAliasO18.createDecoder

        imports =
            case decoder of
                Pipeline ->
                    case version of
                        O17 ->
                            TypeAliasO17.pipelineImports

                        O18 ->
                            TypeAliasO18.pipelineImports

                _ ->
                    case version of
                        O17 ->
                            TypeAliasO17.originalImports

                        O18 ->
                            TypeAliasO18.originalImports

        encoder =
            case version of
                O17 ->
                    TypeAliasO17.createEncoder

                O18 ->
                    TypeAliasO18.createEncoder

        extra =
            case decoder of
                English ->
                    List.map TypeAlias.formatEnglishTypeAlias aliases

                _ ->
                    []

        output =
            [ [ imports ]
            , formattedAliases
            , List.map decoderCreator formattedAliases
            , List.map encoder formattedAliases
            , extra
            ]
                |> List.concat
                |> String.join "\n\n"
    in
        Html.textarea
            [ value <| output
            , class [ Output ]
            ]
            []


viewAllDecoder : ElmVersion -> DecoderType -> String -> Html Action
viewAllDecoder version decoderType incoming =
    let
        encoder =
            case version of
                O17 ->
                    TypeAliasO17.createEncoder

                O18 ->
                    TypeAliasO18.createEncoder

        pipelineImports =
            case version of
                O17 ->
                    TypeAliasO17.pipelineImports

                O18 ->
                    TypeAliasO18.pipelineImports

        pipeLineDecoder =
            case version of
                O17 ->
                    TypeAliasO17.createPipelineDecoder

                O18 ->
                    TypeAliasO18.createPipelineDecoder

        originalDecoder =
            case version of
                O17 ->
                    TypeAliasO17.createDecoder

                O18 ->
                    TypeAliasO18.createDecoder

        originalImports =
            case version of
                O17 ->
                    TypeAliasO17.originalImports

                O18 ->
                    TypeAliasO18.originalImports

        alias =
            TypeAlias.typeAliasFromDecoder incoming

        formattedAlias =
            TypeAlias.aliasFormat alias

        output =
            case decoderType of
                Pipeline ->
                    [ pipelineImports
                    , formattedAlias
                    , pipeLineDecoder formattedAlias
                    , encoder formattedAlias
                    ]
                        |> String.join "\n\n"

                Original ->
                    [ originalImports
                    , formattedAlias
                    , originalDecoder formattedAlias
                    , encoder formattedAlias
                    ]
                        |> String.join "\n\n"

                English ->
                    [ originalImports
                    , formattedAlias
                    , originalDecoder formattedAlias
                    , encoder formattedAlias
                    , TypeAlias.formatEnglishTypeAlias alias
                    ]
                        |> String.join "\n\n"
    in
        Html.textarea
            [ value <| output
            , class [ Output ]
            , spellcheck False
            ]
            []


viewTypeAliasStuff : ElmVersion -> String -> Html Action
viewTypeAliasStuff version incoming =
    let
        decoder =
            case version of
                O17 ->
                    TypeAliasO17.createDecoder incoming

                O18 ->
                    TypeAliasO18.createDecoder incoming

        encoder =
            case version of
                O17 ->
                    TypeAliasO17.createEncoder incoming

                O18 ->
                    TypeAliasO18.createEncoder incoming

        output =
            [ decoder
            , encoder
            , TypeAliasO17.createDecoder incoming
                |> TypeAlias.typeAliasFromDecoder
                |> TypeAlias.formatEnglishTypeAlias
            ]
                |> String.join "\n\n"
    in
        Html.textarea
            [ value <| output
            , class [ Output ]
            , spellcheck False
            ]
            []


viewAllUnions : String -> Html Action
viewAllUnions union =
    let
        type_ =
            UnionType.createUnionType union

        output =
            [ UnionType.createTypeFromString type_
            , UnionType.createDecoder type_
            , UnionType.createEncoder type_
            ]
                |> String.join "\n\n"
    in
        Html.textarea
            [ value <| output
            , class [ Output ]
            , spellcheck False
            ]
            []


viewInput : String -> Html Action
viewInput alias =
    Html.textarea
        [ onInput UpdateInput
        , class [ Input ]
        , spellcheck False
        , placeholder "Put a valid JSON object in here! Now try a type alias, an union type, or an old-style decoder!"
        ]
        [ Html.text <| alias ]


radio : (a -> Action) -> a -> a -> String -> Html Action
radio onUpdate selected decoder name =
    span []
        [ input
            [ type_ "radio"
            , Html.Attributes.checked (selected == decoder)
            , onChange (onUpdate decoder)
            ]
            []
        , Html.text name
        ]


viewDecoderTypeInput : DecoderType -> Html Action
viewDecoderTypeInput decoder =
    div
        []
        [ Html.text "Decoder type: "
        , radio UpdateDecoder decoder Original "original"
        , radio UpdateDecoder decoder Pipeline "pipeline"
        , radio UpdateDecoder decoder English "English"
        ]


viewElmVersionInput : ElmVersion -> Html Action
viewElmVersionInput version =
    div
        []
        [ Html.text "Decoder type: "
        , radio ChangeElmVersion version O17 "0.17 or before"
        , radio ChangeElmVersion version O18 "0.18 or after"
        ]


viewErrors : List String -> Html Action
viewErrors errors =
    ul
        []
        ((List.map (\error -> li [] [ Html.text error ]) errors))


aliasCss : Css.Snippet
aliasCss =
    (.) Alias
        [ padding2 (px 20) zero
        , children
            [ Css.input
                [ padding (px 10)
                , marginLeft (px 10)
                , Css.width (px 250)
                ]
            ]
        ]


viewNameSelect : String -> Html Action
viewNameSelect name =
    div
        [ class [ Alias ] ]
        [ label [] [ Html.text "Enter a toplevel alias name here: " ]
        , input
            [ onInput UpdateName
            , style [ ( "top", "0px" ) ]
            , value name
            ]
            [ Html.text <| name ]
        ]


view : Model -> Html Action
view model =
    let
        aliases =
            if String.trim model.input == "" then
                []
            else
                TypeAlias.createTypeAliasesFromString
                    model.input
                    model.name
                    ""
                    |> Debug.log "alias code:"

        mainBody =
            case model.inputType of
                JsonBlob ->
                    [ viewAllAliases model.elmVersion model.input model.decoderType aliases
                      --, viewStatus model.input aliases
                    ]

                TypeAliasType ->
                    [ viewTypeAliasStuff model.elmVersion model.input
                    ]

                UnionType ->
                    [ viewAllUnions model.input
                    ]

                DecoderString ->
                    [ viewAllDecoder model.elmVersion model.decoderType model.input
                    ]
    in
        div
            [ class [ Content ] ]
            ([ Util.stylesheetLink "./elm/homepage.css"
             , case model.inputType of
                JsonBlob ->
                    viewNameSelect model.name

                _ ->
                    span [] []
             , viewElmVersionInput model.elmVersion
             , viewDecoderTypeInput model.decoderType
             , viewInput model.input
             ]
                ++ mainBody
            )


type Action
    = UpdateInput String
    | UpdateName String
    | ChangeElmVersion ElmVersion
    | UpdateDecoder DecoderType
    | Noop


type InputType
    = TypeAliasType
    | UnionType
    | JsonBlob
    | DecoderString


type DecoderType
    = Original
    | Pipeline
    | English


type ElmVersion
    = O17
    | O18


type alias Model =
    { input : String
    , name : String
    , errors : List String
    , inputType : InputType
    , decoderType : DecoderType
    , elmVersion : ElmVersion
    }


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Noop ->
            ( model, Cmd.none )

        UpdateInput input ->
            let
                trimmed =
                    String.trim input
            in
                ( { model
                    | input = trimmed
                    , inputType =
                        if TypeAlias.isUnionType trimmed then
                            UnionType
                        else if TypeAlias.isTypeAlias trimmed then
                            TypeAliasType
                        else if TypeAlias.isDecoder trimmed then
                            DecoderString
                        else
                            JsonBlob
                  }
                , Cmd.none
                )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateDecoder decoder ->
            ( { model | decoderType = decoder }, Cmd.none )

        ChangeElmVersion version ->
            ( { model | elmVersion = version }, Cmd.none )


model : Model
model =
    { input = """"""
    , name = "Something"
    , errors = []
    , inputType = JsonBlob
    , decoderType = Original
    , elmVersion = O18
    }
