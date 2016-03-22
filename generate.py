from __future__ import print_function, unicode_literals

import json
import sys

from type_alias import create_type_alias, find_type_aliases, find_union_types
from decoder import create_decoder, create_encoder, create_union_type_decoder, create_union_type_encoder
from helpers import *




def test():
    testJson = """
 { "name" : "Noah"
 , "age" : 23
 , "location" :
    { "name" : "sweden"
    , "days" : 45
    }
 }
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

    print(create_union_type_decoder('type Action = Run | Hide | Noop'))
    print(create_union_type_encoder('type Action = Run | Hide | Noop'))

def print_everything(some_json, alias_name):
    stuff = json.loads(some_json)
    aliases = create_type_alias(stuff, type_alias_name=alias_name)
    decoders = [create_decoder(alias, has_snakecase=True, prefix='decode') for alias in aliases ]
    encoders = [create_encoder(alias, has_snakecase=True, prefix='encode') for alias in aliases ]

    print('\n'.join(aliases))
    print('\n'.join(decoders))
    print('\n'.join(encoders))

def from_elm_file(file_text):
    aliases = find_type_aliases(file_text)
    unions = find_union_types(file_text)


    decoders = [create_decoder(alias, has_snakecase=True, prefix='decode') for alias in aliases ]
    decoders.extend(create_union_type_decoder(union_type) for union_type in unions)
    encoders = [create_encoder(alias, has_snakecase=True, prefix='encode') for alias in aliases ]
    encoders.extend(create_union_type_encoder(union_type) for union_type in unions)

    print('\n'.join(decoders))
    print('\n'.join(encoders))



text ="""

type Action =
    Run | Hide

type alias Location =
    { name : String
    , days : Int
    }


type alias Person =
    { age : Int
    , name : String
    , location : Location
    }

location =
    { name = "Noah"
    , days = 45
    }

view =
    div [] []
"""


def message(*args):
    print(*args, file=sys.stderr)


def main():
    if len(sys.argv) < 2:
        message('Give me some elm file names and I\'ll give you decoders and encoders')
        message('Or give me some json files and I\'ll give you a type alias and decoders and encoders')
        return


    for arg in sys.argv[1:]:
        if arg.endswith('.elm'):
            message('Generating decoders and encoders for {name}'.format(name=arg))
            with open(arg) as f:
                from_elm_file(f.read())

        if arg.endswith('.json'):
            message('Generating type alias, decoders and encoders from {name}'.format(name=arg))
            with open(arg) as f:
                arg = arg.split('/')[-1]
                name = arg.split('.')[0].capitalize()
                print_everything(f.read(), name)


if __name__ == '__main__':
    main()
