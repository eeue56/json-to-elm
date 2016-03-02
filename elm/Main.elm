module Main where

import TypeAlias
import Types

import String

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import StartApp
import Effects
import Task


viewAlias : String -> Html
viewAlias alias =
    div [] [ text alias ]

viewJson : String -> Html
viewJson json =
    div
        []
        [ text <| "The entered json was: " ++ json ]

viewDecoder : String -> Html
viewDecoder alias =
    textarea
        [ value <| TypeAlias.createDecoder alias
        , style [ ("width", "40%"), ("height", "500px") ]
        ]
        [ text <| TypeAlias.createDecoder alias]

viewAll : List String -> Html
viewAll aliases =
    let
        output =
            String.join "\n\n" (aliases ++ (List.map TypeAlias.createDecoder aliases) ++ (List.map TypeAlias.createEncoder aliases))
    in
        textarea
            [ value <| output
            , style [ ("width", "40%"), ("height", "500px") ]
            ]
            []

viewInput : Signal.Address Action -> String -> Html
viewInput address alias =
    textarea
        [ on "input" targetValue (Signal.message address << UpdateInput)
        , style [ ("width", "40%"), ("height", "500px") ]
        , placeholder "Put a valid JSON object in here!"
        ]
        [ text <| alias ]

viewNameSelect : Signal.Address Action -> String -> Html
viewNameSelect address name =
    div
        []
        [ label [] [ text "Enter a toplevel alias name here: "]
        , input
            [ on "input" targetValue (Signal.message address << UpdateName)
            , style [ ("top", "0px") ]
            ]
            [ text <| name]
        ]

viewErrors : Signal.Address Action -> List String -> Html
viewErrors address errors =
    ul
        []
        ((List.map (\error -> li [] [ text error ]) errors))


view : Signal.Address Action -> Model -> Html
view address model =
    let
        aliases =
            if String.trim model.input == "" then
                []
            else
                TypeAlias.createTypeAliases (Types.toValue model.input) model.name ""
                    |> List.map TypeAlias.aliasFormat
    in
        div
            []
            [ viewNameSelect address model.name
            , viewInput address model.input
            , viewAll aliases
            , viewErrors address model.errors
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
    { input = """
    {"foo": {
     "foo": "adkljasd"}
    }
    """
    , name = ""
    , errors = []
    }

app =
    StartApp.start { init = (model, Effects.none), view = view, update = update, inputs = [] }

main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
