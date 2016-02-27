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


def find_type_aliases(string):
    string = re.sub('[\\n\\r]', '', string)

    grab_type_aliases = re.findall('type alias.+?\=.+?}', string)

    if grab_type_aliases is None or len(grab_type_aliases) == 0:
        return []

    return grab_type_aliases

def find_union_types(string):
    matches = []

    string = string.replace('type alias', 'asdf')
    grab_union_types = re.findall('type .+?\=.+', string, re.DOTALL)

    for match in grab_union_types:
        build_up = []

        for line in match.split('\n'):
            if len(build_up) > 1 and not line.startswith(' '):
                break

            build_up.append(line)

        matches.append(''.join(build_up))

    return matches
