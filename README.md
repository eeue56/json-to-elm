# json-to-elm
Create Elm type aliases and decoders based on JSON input

`print_everything` takes in a string containing a JSON object, and a top-level name for the alias it will generate.

It will then recursively generate aliases and decoders for all the JSON objects in the json object.

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
```
for more examples of this, see the test function