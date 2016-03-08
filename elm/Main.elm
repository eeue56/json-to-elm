module Main where

import Home exposing (model, update, view)
import StartApp
import Effects
import Task
import Json.Decode as Json

app =
    StartApp.start { init = (model, Effects.none), view = view, update = update, inputs = [] }

main =
    app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks =
    app.tasks
