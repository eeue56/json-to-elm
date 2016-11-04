module Home exposing (..)

import TypeAlias exposing (TypeAlias)
import UnionType
import Types

import String
import Util

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Effects
import Task
import Css exposing (..)
import Css.Elements as Css
import Css.Namespace exposing (namespace)
import Html.CssHelpers
import ColorScheme exposing (..)


cssNamespace = "homepage"

{ class, classList, id } = Html.CssHelpers.withNamespace cssNamespace


type CssClasses =
  Content | Input | Output | Alias

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




viewAlias : String -> Html Action
viewAlias alias =
    div [] [ text alias ]

viewJson : String -> Html Action
viewJson json =
    div
        []
        [ text <| "The entered json was: " ++ json ]

viewOutput : String -> Html Action
viewOutput alias =
    node "pre"
        [ value <| TypeAlias.createDecoder alias
        , class [ Output ]
        ]
        [ text <| TypeAlias.createDecoder alias]

viewAllAliases : String -> DecoderType -> List TypeAlias -> Html Action
viewAllAliases incoming decoder aliases =
    let
        formattedAliases =
            List.map TypeAlias.aliasFormat aliases

        decoderCreator =
            case decoder of
                Pipeline ->
                    TypeAlias.createPipelineDecoder
                _ ->
                    TypeAlias.createDecoder

        imports =
            case decoder of
                Pipeline ->
                    TypeAlias.pipelineImports
                _ ->
                    TypeAlias.originalImports

        output =
            [ [ imports ]
            , formattedAliases
            , List.map decoderCreator formattedAliases
            , List.map TypeAlias.createEncoder formattedAliases
            ]
                |> List.concat
                |> String.join "\n\n"
    in
        textarea
            [ value <| output
            , class [ Output ]
            ]
            []

viewAllDecoder : String -> Html Action
viewAllDecoder incoming =
    let

        alias =
            TypeAlias.typeAliasFromDecoder incoming

        formattedAlias =
            TypeAlias.aliasFormat alias

        output =
            [ TypeAlias.pipelineImports
            , formattedAlias
            , TypeAlias.createPipelineDecoder formattedAlias
            , TypeAlias.createEncoder formattedAlias
            ]
                |> String.join "\n\n"
    in
        textarea
            [ value <| output
            , class [ Output ]
            ]
            []

viewTypeAliasStuff : String -> Html Action
viewTypeAliasStuff incoming =
    let
        output =
            [ TypeAlias.createDecoder incoming
            , TypeAlias.createEncoder incoming
            ]
                |> String.join "\n\n"
    in
        textarea
            [ value <| output
            , class [ Output ]
            ]
            []

viewAllUnions : String -> Html Action
viewAllUnions union =
    let
        type' =
            UnionType.createUnionType union

        output =
            [ UnionType.createTypeFromString type'
            , UnionType.createDecoder type'
            , UnionType.createEncoder type'
            ]
                |> String.join "\n\n"

    in
        textarea
            [ value <| output
            , class [ Output ]
            ]
            []

viewInput : Signal.Address Action -> String -> Html Action
viewInput address alias =
    textarea
        [ on "input" targetValue (Signal.message address << UpdateInput)
        , class [ Input ]
        , placeholder "Put a valid JSON object in here! Now try a type alias, an union type, or an old-style decoder!"
        ]
        [ text <| alias ]


radio : Signal.Address Action -> DecoderType -> DecoderType -> String -> Html Action
radio address selected decoder name =
    span []
        [ input
            [ type' "radio"
            , Html.Attributes.checked (selected == decoder)
            , on "change" targetChecked (\_ -> Signal.message address (UpdateDecoder decoder))
            ]
            []
        , text name
        ]

viewDecoderTypeInput : Signal.Address Action -> DecoderType -> Html Action
viewDecoderTypeInput address decoder =
    div
        []
        [ text "Decoder type: "
        , radio address decoder Original "original"
        , radio address decoder Pipeline "pipeline"
        ]

viewErrors : Signal.Address Action -> List String -> Html Action
viewErrors address errors =
    ul
        []
        ((List.map (\error -> li [] [ text error ]) errors))


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


viewStatus : String -> List TypeAlias -> Html Action
viewStatus incoming aliases =
    let
        successes =
            aliases
                |> List.map (\alias ->
                    (alias,
                        Types.unsafeEval
                            alias.name
                            (TypeAlias.runtimeCreateConstructor alias)
                            (TypeAlias.runtimeCreateDecoder alias)
                            (TypeAlias.runtimeCreateEncoder alias)
                            alias.value
                    )
                )
    in
        successes
            |> List.map (\(alias, evaled) ->
                div
                    []
                    [ text <| "Alias " ++ alias.name ++ " parsed:"
                    , text <| toString evaled
                    ]
            )
            |> div []


viewNameSelect : Signal.Address Action -> String -> Html Action
viewNameSelect address name =
    div
        [ class [ Alias ] ]
        [ label [] [ text "Enter a toplevel alias name here: "]
        , input
            [ on "input" targetValue (Signal.message address << UpdateName)
            , style [ ("top", "0px") ]
            , value name
            ]
            [ text <| name]
        ]


view : Signal.Address Action -> Model -> Html Action
view address model =
    let

        aliases =
            if String.trim model.input == "" then
                []
            else
                TypeAlias.createTypeAliases (Types.toValue model.input) model.name ""

        mainBody =
            case model.inputType of
                JsonBlob ->
                    [ viewAllAliases model.input model.decoderType aliases
                    --, viewStatus model.input aliases
                    ]
                TypeAliasType ->
                    [ viewTypeAliasStuff model.input
                    ]
                UnionType ->
                    [ viewAllUnions model.input
                    ]
                DecoderString ->
                    [ viewAllDecoder model.input
                    ]

    in
        div
            [ class [ Content ] ]
            ([ Util.stylesheetLink "./homepage.css"
            , case model.inputType of
                JsonBlob ->
                    viewNameSelect address model.name
                _ ->
                    span [] []
            , viewDecoderTypeInput address model.decoderType
            , viewInput address model.input
            ]
            ++ mainBody)

type Action
    = UpdateInput String
    | UpdateName String
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

type alias Model =
    { input : String
    , name : String
    , errors : List String
    , inputType : InputType
    , decoderType : DecoderType
    }

update : Action -> Model -> (Model, Cmd Action)
update action model =
    case action of
        Noop ->
            (model, Cmd.none)
        UpdateInput input ->
            (
                { model
                    | input = input
                    , inputType =
                        if TypeAlias.isUnionType input then
                            UnionType
                        else if TypeAlias.isTypeAlias input then
                            TypeAliasType
                        else if TypeAlias.isDecoder input then
                            DecoderString
                        else
                            JsonBlob
                }
                , Cmd.none)
        UpdateName name ->
            ( { model | name = name }, Cmd.none)

        UpdateDecoder decoder ->
            ( { model | decoderType = decoder }, Cmd.none)


model =
    { input = """"""
    , name = "Something"
    , errors = []
    , inputType = JsonBlob
    , decoderType = Original
    }
