import re

def convert_underscore_to_camelcase(name):
    split = name.split('_')
    first = split[0]
    return first + ''.join(x.capitalize() or '_' for x in split[1:])

def convert_camelcase_to_underscores(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()
