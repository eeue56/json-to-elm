port module Main exposing (..)

import Tests
import Test.Runner.Node exposing (run, TestProgram)
import Json.Encode exposing (Value)
import Doc.Tests


main : TestProgram
main =
    run emit Doc.Tests.all


port emit : ( String, Value ) -> Cmd msg
