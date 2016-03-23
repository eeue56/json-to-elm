# json-to-elm
Create Elm type aliases and decoders based on JSON input

This project allows you to automate the creation of:

- type aliases from JSON data
- decoders from type aliases and some union types
- encoders from type aliases and some union types

You can run this program as a command line tool:

```elm
Noahs-MacBook-Pro:json-to-elm noah$ python generate.py ../elm-static-site/examples/Users.elm
decodeUser : Decoder User
decodeUser =
    succeed User
        |: ("name" := string)
        |: ("location" := string)
        |: ("age" := int)
encodeUser : User -> Json.Encode.Value
encodeUser record =
    object
        [ ("name", string record.name)
        , ("location", string record.location)
        , ("age", int record.age)
        ]
```

You can also give it a json file.

or you can use the following functions:

`print_everything` takes in a string containing a JSON object, and a top-level name for the alias it will generate.

It will then recursively generate aliases and decoders for all the JSON objects in the json object.

`create_type_alias` takes a json object, and returns a list of type aliases needed for that object

`create_decoder` takes a type alias as a string, along with a prefix to use for custom decoders, and if the encoded fields have snakecase or not, and generates just the decoder for that type alias

`create_encoder` takes a type alias as a string, along with a prefix to use for custom encoders, and if the encoded fields have snakecase or not, and generates just the encoder for that type alias

`create_union_type_decoder` takes a union type definition as a string, and will generate the decoder needed if the json value is a string

`create_union_type_encoder` takes a union type definition as a string, and will generate the encoder needed if the json value is a string



## Example:

```python

print_everything(
"""
 { "name" : "Noah"
 , "age" : 23
 , "location" :
    { "name" : "sweden"
    , "days" : 45
    }
 }
"""
    , alias_name = "Person")

```

generates

```elm

type alias Location =
    { name : String
    , days : Int
    }


type alias Person =
    { age : Int
    , name : String
    , location : Location
    }

decodeLocation : Json.Decode.Decoder Location
decodeLocation =
    Json.Decode.succeed Location
        |: ("name" := Json.Decode.string)
        |: ("days" := Json.Decode.int)

decodePerson : Json.Decode.Decoder Person
decodePerson =
    Json.Decode.succeed Person
        |: ("age" := Json.Decode.int)
        |: ("name" := Json.Decode.string)
        |: ("location" := decodeLocation)

encodeLocation : Location -> Json.Encode.Value
encodeLocation record =
    Json.Encode.object
        [ ("name", Json.Decode.string record.name)
        , ("days", Json.Decode.int record.days)
        ]

encodePerson : Person -> Json.Encode.Value
encodePerson record =
    Json.Encode.object
        [ ("age", Json.Decode.int record.age)
        , ("name", Json.Decode.string record.name)
        , ("location", encodeLocation record.location)
        ]

```
for more examples of this, see the test function

### Union types

```python
print(create_union_type_decoder('type Suit = Hearts | Diamonds | Spades | Clubs'))
print(create_union_type_encoder('type Suit = Hearts | Diamonds | Spades | Clubs'))
```

will print

```elm
decodeSuit : Json.Decode.Decoder Suit
decodeSuit =
    let
        decodeToType string =
            case string of
                "Hearts" -> Result.Ok Hearts
                "Diamonds" -> Result.Ok Diamonds
                "Spades" -> Result.Ok Spades
                "Clubs" -> Result.Ok Clubs
                _ -> Result.Err ("Not valid pattern for decoder to Suit. Pattern: " ++ (toString string))
    in
        Json.Decode.customDecoder Json.Decode.string decodeToType

encodeSuit : Suit -> Json.Encode.Value
encodeSuit item =
    case item of
        Hearts -> Json.Encode.string "Hearts"
        Diamonds -> Json.Encode.string "Diamonds"
        Spades -> Json.Encode.string "Spades"
        Clubs -> Json.Encode.string "Clubs"
```
