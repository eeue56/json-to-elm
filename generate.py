from __future__ import print_function, unicode_literals

import json

from type_alias import create_type_alias
from decoder import create_decoder, create_encoder
from helpers import *




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
