module Types where

import Json.Encode as Json
import Native.Types


suggestType : Json.Value -> String
suggestType =
    Native.Types.makeGuessAtType

toValue : String -> Json.Value
toValue =
    Native.Types.toValue

keys : Json.Value -> List String
keys =
    Native.Types.keys

get : String -> Json.Value -> Maybe Json.Value
get =
    Native.Types.get

unsafeGet : String -> Json.Value -> Json.Value
unsafeGet =
    Native.Types.unsafeGet
