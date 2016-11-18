module Main exposing (..)

import Home exposing (model, update, view)
import Html


main : Program Never Home.Model Home.Action
main =
    Html.program
        { init = ( model, Cmd.none )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
