module Stylesheets exposing (..)

import Css.File exposing (..)
import Home


port files : CssFileStructure
port files =
    toFileStructure
        [ ( "homepage.css", compile Home.css ) ]
