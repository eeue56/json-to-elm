import re

from helpers import *

try:
    xrange(5)
    STR_TYPES = (str, unicode)
except NameError:
    STR_TYPES = (str, bytes)

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
