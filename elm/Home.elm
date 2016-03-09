module Home where

import TypeAlias exposing (TypeAlias)
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

{ class, classList, id } = Html.CssHelpers.namespace cssNamespace

textStuff =
    """
{
    "name": "Dave",
    "age" : 15,
    "location" : {
        "name" : "Sweden",
        "days" : 25.5
    }
}
    """


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

viewAll : String -> List TypeAlias -> Html
viewAll incoming aliases =
    let
        formattedAliases =
            List.map TypeAlias.aliasFormat aliases

        output =
            [ formattedAliases
            , List.map TypeAlias.createDecoder formattedAliases
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

    in
        div
            [ class [ Content ] ]
            [ viewNameSelect address model.name
            , viewInput address model.input
            , viewAll model.input aliases
            , viewStatus model.input aliases
            , Util.stylesheetLink "/homepage.css"
            ]

type Action
    = UpdateInput String
    | UpdateName String
    | Noop

type alias Model =
    { input : String
    , name : String
    , errors : List String
    }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        Noop ->
            (model, Effects.none)
        UpdateInput input ->
            ( { model | input = input }, Effects.none)
        UpdateName name ->
            ( { model | name = name }, Effects.none)


model =
    { input = """{
  "name" :"noah",
  "age" : [5]
}"""
    , name = "User"
    , errors = []
    }
