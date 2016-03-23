import re

from helpers import *


KNOWN_DECODERS = [
    'maybe',
    'list' ,
    'int' ,
    'float' ,
    'bool',
    'string' ]

JSON_DECODE = 'Json.Decode.'
JSON_ENCODE = 'Json.Encode.'

def get_type_alias_name(string):
    grab_type_name = re.search('type alias(.+)\=', string)

    if grab_type_name is None or len(grab_type_name.groups()) == 0:
        raise Exception("Can't find type alias declaration")

    groups = grab_type_name.groups()

    if len(groups) > 1:
        raise "Please only give me one type alias at a time"

    return groups[0].strip()

def get_union_type_name(string):
    grab_type_name = re.search('type (.+)\=', string)

    if grab_type_name is None or len(grab_type_name.groups()) == 0:
        raise Exception("Can't find type declaration")

    groups = grab_type_name.groups()

    if len(groups) > 1:
        raise "Please only give me one type at a time"

    return groups[0].strip()


def get_fields(string):
    grab_fields = re.match(".*{(.+)\}", string)

    if grab_fields is None or len(grab_fields.groups()) == 0:
        raise Exception("Please give me a field I can work with")

    groups = grab_fields.groups()

    return groups[0].split(',')

def get_constructors(string):
    string = string[string.index('=') + 1:]
    return [part.strip() for part in string.split('|')]

def field_name_and_type(string):
    splitted = string.split(':')

    return (splitted[0].strip(), splitted[1].strip())

def make_guess_at_codec(string):
    return string.lower()

def prefix_codec(prefix, value, default_module=JSON_DECODE):
    parts = value.split()

    return ' '.join(
        default_module + value
            if value in KNOWN_DECODERS
            else prefix + value.capitalize()
            for value in parts
    )

def suffix_codec(suffix, value):
    parts = value.split()

    return ' '.join(
        value
            if value in KNOWN_DECODERS
            else value + suffix
            for value in parts
    )

def create_decoder(string, has_snakecase=False, prefix=None, suffix=None):
    string = re.sub('[\\n\\r]', '', string)
    type_name = get_type_alias_name(string)
    fields = [field_name_and_type(f) for f in get_fields(string)]

    if has_snakecase:
        fields = [
            (convert_camelcase_to_underscores(name), value)
                for name, value in fields
        ]

    fields = [(name, make_guess_at_codec(type)) for name, type in fields]

    if prefix is not None:
        fields = [ (name, prefix_codec(prefix, value)) for name, value in fields ]

    if suffix is not None:
        fields = [ (name, suffix_codec(suffix, value)) for name, value in fields ]


    formattedFields ='\n        '.join(
        '|: ("{name}" := {type})'
            .format(
                name=name,
                type=type
            )
            for (name, type) in fields)


    output = """
decode{type_name} : Json.Decode.Decoder {type_name}
decode{type_name} =
    Json.Decode.succeed {type_name}
        {fields}

    """.format(type_name=type_name, fields=formattedFields)

    return output.strip()


def create_encoder(string, has_snakecase=False, prefix=None, suffix=None):
    string = re.sub('[\\n\\r]', '', string)
    type_name = get_type_alias_name(string)
    original_fields = [field_name_and_type(f) for f in get_fields(string)]
    fields = original_fields[:]

    if has_snakecase:
        fields = [
            (convert_camelcase_to_underscores(name), value)
                for name, value in fields
        ]

    fields = [(name, make_guess_at_codec(type)) for name, type in fields]

    if prefix is not None:
        fields = [ (name, prefix_codec(prefix, value, JSON_ENCODE)) for name, value in fields ]

    if suffix is not None:
        fields = [ (name, suffix_codec(suffix, value)) for name, value in fields ]

    formatted_fields ='\n        , '.join(
        '("{name}", {type} record.{original_name})'
            .format(
                name=encoded_name,
                type=' <| '.join(encoder.split(' ')),
                original_name=original_name
            )
            for ((encoded_name, encoder), (original_name, _)) in zip(fields, original_fields))


    output = """
encode{type_name} : {type_name} -> Json.Encode.Value
encode{type_name} record =
    Json.Encode.object
        [ {fields}
        ]
    """.format(type_name=type_name, fields=formatted_fields)

    return output.strip()


def create_union_type_decoder(string, has_snakecase=False):
    """
        string is a union type that looks like
            type Action = Noop | Run
    """
    string = re.sub('[\\n\\r]', '', string)

    type_name = get_union_type_name(string)
    constructors = get_constructors(string)

    formatted_constructors = '\n                '.join(
        '"{constructor}" -> Result.Ok {constructor}'.format(constructor=constructor) for constructor in constructors
        )


    output = """

decode{type_name} : Json.Decode.Decoder {type_name}
decode{type_name} =
    let
        decodeToType string =
            case string of
                {patterns}
                _ -> Result.Err ("Not valid pattern for decoder to {type_name}. Pattern: " ++ (toString string))
    in
        Json.Decode.customDecoder Json.Decode.string decodeToType
""".format(type_name=type_name, patterns=formatted_constructors)

    return output.strip()

def create_union_type_encoder(string, has_snakecase=False):
    """
        string is a union type that looks like
            type Action = Noop | Run
    """
    string = re.sub('[\\n\\r]', '', string)

    type_name = get_union_type_name(string)
    constructors = get_constructors(string)

    formatted_constructors = '\n        '.join(
        '{constructor} -> Json.Encode.string "{constructor}"'.format(constructor=constructor) for constructor in constructors
        )


    output = """

encode{type_name} : {type_name} -> Json.Encode.Value
encode{type_name} item =
    case item of
        {patterns}
""".format(type_name=type_name, patterns=formatted_constructors)

    return output.strip()
