module Util exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Encode exposing (string)


stylesheetLink : String -> Html msg
stylesheetLink url =
    Html.node
        "link"
        [ property "rel" (string "stylesheet")
        , property "type" (string "text/css")
        , property "href" (string url)
        ]
        []
