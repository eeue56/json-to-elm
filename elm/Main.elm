module Main exposing (..)

import Home exposing (model, update, view)
import Task
import Json.Decode as Json

main : Program Never
main =
    Html.App.program
        { init = (model, Cmd.none)
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
