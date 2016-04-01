module Home where

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




viewAlias : String -> Html
viewAlias alias =
    div [] [ text alias ]

viewJson : String -> Html
viewJson json =
    div
        []
        [ text <| "The entered json was: " ++ json ]

viewOutput : String -> Html
viewOutput alias =
    node "pre"
        [ value <| TypeAlias.createDecoder alias
        , class [ Output ]
        ]
        [ text <| TypeAlias.createDecoder alias]

viewAllAliases : String -> List TypeAlias -> Html
viewAllAliases incoming aliases =
    let
        formattedAliases =
            List.map TypeAlias.aliasFormat aliases

        output =
            [ [ TypeAlias.imports ]
            , formattedAliases
            , List.map TypeAlias.createPipelineDecoder formattedAliases
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

viewAllDecoder : String -> Html
viewAllDecoder incoming =
    let

        alias =
            TypeAlias.typeAliasFromDecoder incoming

        formattedAlias =
            TypeAlias.aliasFormat alias

        output =
            [ TypeAlias.imports
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

viewTypeAliasStuff : String -> Html
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

viewAllUnions : String -> Html
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

viewInput : Signal.Address Action -> String -> Html
viewInput address alias =
    textarea
        [ on "input" targetValue (Signal.message address << UpdateInput)
        , class [ Input ]
        , placeholder "Put a valid JSON object in here!"
        ]
        [ text <| alias ]

viewErrors : Signal.Address Action -> List String -> Html
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


viewStatus : String -> List TypeAlias -> Html
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


viewNameSelect : Signal.Address Action -> String -> Html
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


view : Signal.Address Action -> Model -> Html
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
                    [ viewAllAliases model.input aliases
                    , viewStatus model.input aliases
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
            ([ Util.stylesheetLink "/homepage.css"
            , case model.inputType of
                JsonBlob ->
                    viewNameSelect address model.name
                _ ->
                    span [] []
            , viewInput address model.input
            ]
            ++ mainBody)

type Action
    = UpdateInput String
    | UpdateName String
    | Noop

type InputType
    = TypeAliasType
    | UnionType
    | JsonBlob
    | DecoderString

type alias Model =
    { input : String
    , name : String
    , errors : List String
    , inputType : InputType
    }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        Noop ->
            (model, Effects.none)
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
                , Effects.none)
        UpdateName name ->
            ( { model | name = name }, Effects.none)


model =
    { input = """{
  "name" :"noah",
  "age" : [5]
}"""
    , name = "User"
    , errors = []
    , inputType = JsonBlob
    }
