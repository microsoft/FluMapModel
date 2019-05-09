import json
import hashlib
from flask_marshmallow import Marshmallow

ma = None

def get_marshmallow():
    return ma

def set_marshmallow(app):
    global ma
    # Initialize Marshmallow
    ma = Marshmallow(app)
    return ma

def get_model_id(query):
    if type(query) is dict:
        query_json = dict(model_type=query['model_type'],
                          observed=sorted(query['observed'], key=str.lower))
        json_str = json.dumps(query_json, sort_keys=True, separators=(',', ':'))
    else:
        json_str = query
    m = hashlib.md5()
    m.update(json_str.encode('ascii'))
    return m.hexdigest()