module Home exposing (..)

import TypeAlias exposing (TypeAlias)
import UnionType
import Types
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
onChange msg = Html.Events.on "change" (Json.Decode.succeed msg)

viewAlias : String -> Html Action
viewAlias alias =
    div [] [ Html.text alias ]


viewJson : String -> Html Action
viewJson json =
    div
        []
        [ Html.text <| "The entered json was: " ++ json ]


viewOutput : String -> Html Action
viewOutput alias =
    node "pre"
        [ value <| TypeAlias.createDecoder alias
        , class [ Output ]
        ]
        [ Html.text <| TypeAlias.createDecoder alias ]


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
        Html.textarea
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
        Html.textarea
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
        Html.textarea
            [ value <| output
            , class [ Output ]
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
            ]
            []


viewInput : String -> Html Action
viewInput alias =
    Html.textarea
        [ onInput UpdateInput
        , class [ Input ]
        , placeholder "Put a valid JSON object in here! Now try a type alias, an union type, or an old-style decoder!"
        ]
        [ Html.text <| alias ]


radio : DecoderType -> DecoderType -> String -> Html Action
radio selected decoder name =
    span []
        [ input
            [ type_ "radio"
            , Html.Attributes.checked (selected == decoder)
            , onChange (UpdateDecoder decoder)
            ]
            []
        , Html.text name
        ]


viewDecoderTypeInput : DecoderType -> Html Action
viewDecoderTypeInput decoder =
    div
        []
        [ Html.text "Decoder type: "
        , radio decoder Original "original"
        , radio decoder Pipeline "pipeline"
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


viewStatus : String -> List TypeAlias -> Html Action
viewStatus incoming aliases =
    let
        successes =
            aliases
                |> List.map
                    (\alias ->
                        ( alias
                        , Types.unsafeEval
                            alias.name
                            (TypeAlias.runtimeCreateConstructor alias)
                            (TypeAlias.runtimeCreateDecoder alias)
                            (TypeAlias.runtimeCreateEncoder alias)
                            alias.value
                        )
                    )
    in
        successes
            |> List.map
                (\( alias, evaled ) ->
                    div
                        []
                        [ Html.text <| "Alias " ++ alias.name ++ " parsed:"
                        , Html.text <| toString evaled
                        ]
                )
            |> div []


viewNameSelect : String -> Html Action
viewNameSelect  name =
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
                    viewNameSelect model.name

                _ ->
                    span [] []
             , viewDecoderTypeInput model.decoderType
             , viewInput model.input
             ]
                ++ mainBody
            )


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


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Noop ->
            ( model, Cmd.none )

        UpdateInput input ->
            ( { model
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
            , Cmd.none
            )

        UpdateName name ->
            ( { model | name = name }, Cmd.none )

        UpdateDecoder decoder ->
            ( { model | decoderType = decoder }, Cmd.none )

model : Model
model =
    { input = """"""
    , name = "Something"
    , errors = []
    , inputType = JsonBlob
    , decoderType = Original
    }
