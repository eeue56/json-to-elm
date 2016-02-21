# json-to-elm
Create Elm type aliases and decoders based on JSON input


Two parts to this script. 

## Creating type aliases from json

Make a type alias based on a json object. Works for some things like strings, ints, float, maybes if a value is null, and will recursively generate type aliases. E.g if your `assignment` type alias has a field which is another object, and the name of that field is `course`, then it will generate a type alias called `Course`

`createTypeAlias` takes a Python dict. So if you want to load, just do

```python
# testJson : String
# stuff : Dict
stuff = json.loads(testJson)
# aliases : List String
aliases = createTypeAlias(stuff, typeAliasName='Assignment')
```

will generate

```elm
type alias Course =
    { userId : Int
, name : String
, completed : Int
, premiumLicenseId : Maybe a
, updatedAt : String
, inviteCode : String
, id : Int
, edmodoId : Maybe a
, premium : Int
, createdAt : String
    }

type alias Assignment =
    { someList : List Int
, name : String
, startImmediately : Int
, hasStarted : Int
, due : String
, scrambleQuestionOrder : Int
, acceptLate : Int
, course : Course
, previousTestId : Maybe a
, points : Int
, startOn : String
, mode : Int
, questions : Int
, type : String
    }
```

## Generating decoders from type aliases

Takes in a type alias as a string. Takes `has_snakecase` as optional argument, changing field names to be snake case instead of camelcase for decoding purposes.
Works for most things. Expects decoders to have the lowercase name of the thing it's decoding.

```python
createDecoder(assignment_alias, has_snakecase=True)
```
will generate
```elm
decodeAssignment : Decoder TempAssignment
decodeAssignment =
    succeed TempAssignment
        |: ("name" := string)
        |: ("start_immediately" := int)
        |: ("has_started" := int)
        |: ("due" := string)
        |: ("scramble_question_order" := int)
        |: ("accept_late" := int)
        |: ("course" := decodeCourse)
        |: ("previous_test_id" := maybe int)
        |: ("points" := int)
        |: ("start_on" := string)
        |: ("mode" := int)
        |: ("questions" := int)
        |: ("type" := string)
```
for more examples of this, see the test function
