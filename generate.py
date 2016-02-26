from __future__ import print_function, unicode_literals

import json

from type_alias import create_type_alias
from decoder import create_decoder, create_encoder, create_union_type_decoder
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

def print_everything(some_json, alias_name):
    stuff = json.loads(some_json)
    aliases = create_type_alias(stuff, type_alias_name=alias_name)
    decoders = [create_decoder(alias, has_snakecase=True, prefix='decode') for alias in aliases ]
    encoders = [create_encoder(alias, has_snakecase=True, prefix='encode') for alias in aliases ]

    print('\n'.join(aliases))
    print('\n'.join(decoders))
    print('\n'.join(encoders))


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

    print(create_union_type_decoder('type Action = Run | Hide | Noop'))
