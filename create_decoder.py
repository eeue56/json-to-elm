from __future__ import print_function, unicode_literals

import re
import json


try:
    xrange(5)
    STR_TYPES = (str, unicode)
except NameError:
    STR_TYPES = (str, bytes)

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

def make_guess_at_type(item, depth=0):

    if depth > 20:
        print('Too much recursiveness!')
        raise Exception('You got greedy and dug too deep')

    if item is None:
        return 'Maybe _Unknown'

    if isinstance(item, bool):
        return 'Bool'

    if isinstance(item, STR_TYPES):
        return 'String'

    if isinstance(item, int):
        return 'Int'

    if isinstance(item, float):
        return 'Float'

    if isinstance(item, list):
        if len(item) == 0:
            return 'List a'
        return 'List ' + make_guess_at_type(item[0], depth=depth+1)

    ## if it's a complicated item, instead of making a dict, return nothing

    if isinstance(item, dict):
        return 'Something'

    return 'Unknown'

def create_type_alias(stuff, type_alias_name):
    fields = []

    extra_aliases = []

    for name, value in stuff.items():
        type = make_guess_at_type(value)

        if type == 'Something':
            extra_aliases.extend(create_type_alias(value, type_alias_name=name.capitalize()))
            type = name.capitalize()

        name = convert_underscore_to_camelcase(name)

        fields.append('{name} : {type}'.format(name=name, type=type))

    joined_fields = '\n    , '.join(fields)

    extra_aliases.append('''
type alias {name} =
    {{ {fields}
    }}
'''.format(name=type_alias_name, fields=joined_fields)
    )

    return extra_aliases

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


def convert_underscore_to_camelcase(name):
    split = name.split('_')
    first = split[0]
    return first + ''.join(x.capitalize() or '_' for x in split[1:])

def convert_camelcase_to_underscores(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

def test():
    testJson = """
{"some_list": [1, 2, 3], "name":"ghj2True",
"course":{"completed":false,"created_at":"2016-01-27T09:25:52-06:00","edmodo_id":null,"id":282074,"invite_code":"ex8w7fdd","name":"dasd","premium_license_id":null,"updated_at":"2016-02-18T06:25:09-06:00","user_id":3439703,"premium":false},"start_on":"2016-02-18T09:37:02-06:00","start_immediately":true,"scramble_question_order":true,"accept_late":true,"type":"UnitDiagnostic","previous_test_id":null,"points":0,"has_started":true,"due":"2016-02-27T23:59:00-06:00","questions":20,"mode":1}
    """

    stuff = json.loads(testJson)
    print('Creating type alias')

    aliases = create_type_alias(stuff, type_alias_name='Assignment')
    print('\n'.join(aliases))

    print('Creating decoder')
    decoders = []
    for alias in aliases:
        decoder = create_decoder(alias, has_snakecase=True, prefix='decode')
        decoders.append(decoder)

    print('\n'.join(decoders))

def print_everything(some_json, alias_name):
    stuff = json.loads(some_json)
    aliases = create_type_alias(stuff, type_alias_name=alias_name)
    decoders = [create_decoder(alias, has_snakecase=True, prefix='decode') for alias in aliases ]

    print('\n'.join(aliases))
    print('\n'.join(decoders))

if __name__ == '__main__':
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
