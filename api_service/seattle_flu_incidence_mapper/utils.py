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
    query['observed'] = sorted(query['observed'], key=str.lower)
    m = hashlib.md5()
    m.update(json.dumps(query, sort_keys=True, separators=(',',':')).encode('ascii'))
    return m.hexdigest()