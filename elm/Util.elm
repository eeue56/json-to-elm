module Util where

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode exposing (string)

stylesheetLink : String -> Html
stylesheetLink url =
  Html.node
        "link"
        [ property "rel" (string "stylesheet")
        , property "type" (string "text/css")
        , property "href" (string url)
        ]
        []
