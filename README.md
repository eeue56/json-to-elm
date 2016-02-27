# json-to-elm
Create Elm type aliases and decoders based on JSON input


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

decodeLocation : Decoder Location
decodeLocation =
    succeed Location
        |: ("name" := string)
        |: ("days" := int)
decodePerson : Decoder Person
decodePerson =
    succeed Person
        |: ("age" := int)
        |: ("name" := string)
        |: ("location" := decodeLocation)

encodeLocation : Location -> Json.Encode.Value
encodeLocation record =
    object
        [ ("name", string record.name)
        , ("days", int record.days)
        ]
encodePerson : Person -> Json.Encode.Value
encodePerson record =
    object
        [ ("age", int record.age)
        , ("name", string record.name)
        , ("location", encodeLocation record.location)
        ]

```
for more examples of this, see the test function


```python
print(create_union_type_decoder('type Action = Run | Hide | Noop'))
print(create_union_type_encoder('type Action = Run | Hide | Noop'))
```

will print

```elm
decodeAction : Decoder Action
decodeAction =
    string
        |> (\string ->
            case string of
                "Run" -> Run
                "Hide" -> Hide
                "Noop" -> Noop
        )
encodeAction : Action -> Json.Encode.Value
encodeAction item =
    case item of
        Run -> string "Run"
        Hide -> string "Hide"
        Noop -> string "Noop"
```
