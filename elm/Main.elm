module Main where

import TypeAlias
import Types

import Html exposing (..)
import Html.Attributes exposing (..)
import StartApp
import Effects

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


main =
    div
        []

        (viewJson textStuff :: (List.map viewAlias aliases))
