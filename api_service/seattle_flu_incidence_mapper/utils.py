from flask_marshmallow import Marshmallow

ma = None

def get_marshmallow():
    return ma

def set_marshmallow(app):
    global ma
    # Initialize Marshmallow
    ma = Marshmallow(app)
    return ma
