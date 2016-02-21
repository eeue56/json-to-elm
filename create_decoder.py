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

def getTypeName(string):
    grabTypeName = re.search('type alias(.+)\=', string)

    if grabTypeName is None or len(grabTypeName.groups()) == 0:
        raise Exception("Can't find type alias declaration")

    groups = grabTypeName.groups()

    if len(groups) > 1:
        raise "Please only give me one type alias at a time"

    return groups[0].strip()


def getFields(string):
    grabFields = re.match(".*{(.+)\}", string)

    if grabFields is None or len(grabFields.groups()) == 0:
        raise Exception("Please give me a field I can work with")

    groups = grabFields.groups()

    return groups[0].split(',')

def fieldNameAndType(string):
    splitted = string.split(':')

    return (splitted[0].strip(), splitted[1].strip())

def makeGuessAtDecoder(string):
    return string.lower()

def makeGuessAtType(item, depth=0):

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
        return 'List ' + makeGuessAtType(item[0], depth=depth+1)

    ## if it's a complicated item, instead of making a dict, return nothing

    if isinstance(item, dict):
        return 'Something'

    return 'Unknown'

def createTypeAlias(stuff, typeAliasName):
    fields = []

    extra_aliases = []

    for name, value in stuff.items():
        type = makeGuessAtType(value)

        if type == 'Something':
            extra_aliases.extend(createTypeAlias(value, typeAliasName=name.capitalize()))
            type = name.capitalize()

        name = convertUnderscoreToCamelcase(name)

        fields.append('{name} : {type}'.format(name=name, type=type))

    joined_fields = '\n, '.join(fields)

    extra_aliases.append('''
type alias {name} =
    {{ {fields}
    }}
'''.format(name=typeAliasName, fields=joined_fields)
    )

    return extra_aliases

def prefixDecoder(prefix, value):
    parts = value.split()

    return ' '.join(
        value
            if value in KNOWN_DECODERS
            else prefix + value.capitalize()
            for value in parts
    )

def suffixDecoder(suffix, value):
    parts = value.split()

    return ' '.join(
        value
            if value in KNOWN_DECODERS
            else value + suffix
            for value in parts
    )

def createDecoder(string, has_snakecase=False, prefix=None, suffix=None):
    string = re.sub('[\\n\\r]', '', string)
    typeName = getTypeName(string)
    fields = [fieldNameAndType(f) for f in getFields(string)]

    if has_snakecase:
        fields = [
            (convertCamelcaseToUnderscores(name), value)
                for name, value in fields
        ]

    fields = [(name, makeGuessAtDecoder(type)) for name, type in fields]

    if prefix is not None:
        fields = [ (name, prefixDecoder(prefix, value)) for name, value in fields ]

    if suffix is not None:
        fields = [ (name, suffixDecoder(suffix, value)) for name, value in fields ]


    formattedFields ='\n        '.join(
        '|: ("{name}" := {type})'
            .format(
                name=name,
                type=type
            )
            for (name, type) in fields)


    output = """
decode{typeName} : Decoder {typeName}
decode{typeName} =
    succeed {typeName}
        {fields}

    """.format(typeName=typeName, fields=formattedFields)

    return output.strip()


def convertUnderscoreToCamelcase(name):
    split = name.split('_')
    first = split[0]
    return first + ''.join(x.capitalize() or '_' for x in split[1:])

def convertCamelcaseToUnderscores(name):
    s1 = re.sub('(.)([A-Z][a-z]+)', r'\1_\2', name)
    return re.sub('([a-z0-9])([A-Z])', r'\1_\2', s1).lower()

def test():
    testJson = """
{"some_list": [1, 2, 3], "name":"ghj2True",
"course":{"completed":false,"created_at":"2016-01-27T09:25:52-06:00","edmodo_id":null,"id":282074,"invite_code":"ex8w7fdd","name":"dasd","premium_license_id":null,"updated_at":"2016-02-18T06:25:09-06:00","user_id":3439703,"premium":false},"start_on":"2016-02-18T09:37:02-06:00","start_immediately":true,"scramble_question_order":true,"accept_late":true,"type":"UnitDiagnostic","previous_test_id":null,"points":0,"has_started":true,"due":"2016-02-27T23:59:00-06:00","questions":20,"mode":1}
    """

    stuff = json.loads(testJson)
    print('Creating type alias')

    aliases = createTypeAlias(stuff, typeAliasName='Assignment')
    print('\n'.join(aliases))

    print('Creating decoder')
    decoders = []
    for alias in aliases:
        decoder = createDecoder(alias, has_snakecase=True, prefix='decode')
        decoders.append(decoder)

    print('\n'.join(decoders))

def printEverything(someJson):
    stuff = json.loads(someJson)
    aliases = createTypeAlias(stuff, typeAliasName='Assignment')
    decoders = [createDecoder(alias, has_snakecase=True, prefix='decode') for alias in aliases ]

    print('\n'.join(aliases))
    print('\n'.join(decoders))

if __name__ == '__main__':
    printEverything(
        """{"name":"ghj2True","course":{"completed":false,"created_at":"2016-01-27T09:25:52-06:00","edmodo_id":null,"id":282074,"invite_code":"ex8w7fdd","name":"dasd","premium_license_id":null,"updated_at":"2016-02-18T06:25:09-06:00","user_id":3439703,"premium":false},"start_on":"2016-02-18T09:37:02-06:00","start_immediately":true,"scramble_question_order":true,"accept_late":true,"type":"UnitDiagnostic","previous_test_id":null,"points":0,"has_started":true,"due":"2016-02-27T23:59:00-06:00","questions":20,"mode":1}"""
        )
