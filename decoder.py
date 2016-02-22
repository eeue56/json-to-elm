import re

from helpers import *


KNOWN_DECODERS = [
    'maybe',
    'list' ,
    'int' ,
    'float' ,
    'bool',
    'string' ]

def get_type_name(string):
    grab_type_name = re.search('type alias(.+)\=', string)

    if grab_type_name is None or len(grab_type_name.groups()) == 0:
        raise Exception("Can't find type alias declaration")

    groups = grab_type_name.groups()

    if len(groups) > 1:
        raise "Please only give me one type alias at a time"

    return groups[0].strip()

def get_fields(string):
    grabFields = re.match(".*{(.+)\}", string)

    if grabFields is None or len(grabFields.groups()) == 0:
        raise Exception("Please give me a field I can work with")

    groups = grabFields.groups()

    return groups[0].split(',')

def field_name_and_type(string):
    splitted = string.split(':')

    return (splitted[0].strip(), splitted[1].strip())

def make_guess_at_decoder(string):
    return string.lower()

def prefix_decoder(prefix, value):
    parts = value.split()

    return ' '.join(
        value
            if value in KNOWN_DECODERS
            else prefix + value.capitalize()
            for value in parts
    )

def suffix_decoder(suffix, value):
    parts = value.split()

    return ' '.join(
        value
            if value in KNOWN_DECODERS
            else value + suffix
            for value in parts
    )

def create_decoder(string, has_snakecase=False, prefix=None, suffix=None):
    string = re.sub('[\\n\\r]', '', string)
    type_name = get_type_name(string)
    fields = [field_name_and_type(f) for f in get_fields(string)]

    if has_snakecase:
        fields = [
            (convert_camelcase_to_underscores(name), value)
                for name, value in fields
        ]

    fields = [(name, make_guess_at_decoder(type)) for name, type in fields]

    if prefix is not None:
        fields = [ (name, prefix_decoder(prefix, value)) for name, value in fields ]

    if suffix is not None:
        fields = [ (name, suffix_decoder(suffix, value)) for name, value in fields ]


    formattedFields ='\n        '.join(
        '|: ("{name}" := {type})'
            .format(
                name=name,
                type=type
            )
            for (name, type) in fields)


    output = """
decode{type_name} : Decoder {type_name}
decode{type_name} =
    succeed {type_name}
        {fields}

    """.format(type_name=type_name, fields=formattedFields)

    return output.strip()
