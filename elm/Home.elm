module Home where

import TypeAlias
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


css =
  (stylesheet << namespace "main")
    [ Css.body
        [ backgroundColor (rgb 30 90 90)
        ]
    ]

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


view : Signal.Address Action -> Model -> Html
view address model =
    let
        aliases =
            if String.trim model.input == "" then
                []
            else
                TypeAlias.createTypeAlias (Types.toValue model.input) model.name
    in
        div
            []
            [ viewNameSelect address model.name
            , viewInput address model.input
            , viewAll aliases
            ]

type Action
    = UpdateInput String
    | UpdateName String
    | Noop

type alias Model =
    { input : String
    , name : String
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
    { input = ""
    , name = ""
    }
