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



aliases = TypeAlias.createTypeAlias (Types.toValue textStuff) "User"

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
            String.join "\n\n" (aliases ++ (List.map TypeAlias.createDecoder aliases))
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
        ]
        [ text <| model.input ]


view : Signal.Address Action -> Model -> Html
view address model =
    let
        aliases =
            TypeAlias.createTypeAlias (Types.toValue model.input) "User"
    in
    div
        []
        [ viewInput address model.input, viewAll aliases ]

type Action
    = UpdateInput String
    | Noop

type alias Model =
    { input : String
    }

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
    case action of
        Noop ->
            (model, Effects.none)
        UpdateInput input ->
            ( { model | input = input }, Effects.none)

model =
    { input = ""}

app =
    StartApp.start { init = (model, Effects.none), view = view, update = update, inputs = [] }

main =
    app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
