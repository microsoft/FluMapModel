import time

import click
import six
import json
import os
import functools
from werkzeug.exceptions import Unauthorized
from flask import current_app
from jose import JWTError, jwt
from werkzeug.test import EnvironBuilder

from seattle_flu_incidence_mapper.config import app

DEFAULT_TOKEN_DB_PATH = os.environ.get("API_TOKEN_DB", "/model_tokens")
true_values = ['1', 'y', 'yes', 't', 'true']


@functools.lru_cache(maxsize=1)
def get_token_db():
    """
    Load our token db from json file

    we cache this function with lru_cache an no arguments so effectively it is a singleton
    """
    if os.environ.get('DEBUG', '0').lower() in true_values or os.environ.get('DEBUG_TOKENS', '0').lower() in true_values:
        return { 'sample_token': {'user': 'abc', 'role':'admin'}}
    return json.load(open(os.path.join(DEFAULT_TOKEN_DB_PATH, "tokens.json"), "r"))


def generate_token(user_id):
    timestamp = _current_timestamp()
    payload = {
        "iss": current_app.config['JWT_ISSUER'],
        "iat": int(timestamp),
        "exp": int(timestamp + current_app.config['JWT_LIFETIME_SECONDS']),
        "sub": str(user_id),
    }

    return jwt.encode(payload, current_app.config['JWT_SECRET'], algorithm=current_app.config['JWT_ALGORITHM'])
EnvironBuilder

@app.cli.command('generate-token')
@click.argument('user_id')
@click.argument('lifetime', default=60*60*24*364)
def generate_token_cli(user_id, lifetime):
    current_app.config['JWT_LIFETIME_SECONDS'] = lifetime
    token = generate_token(user_id)
    print(f"User {user_id}'s token is {token}")


def decode_token(token):
    try:
        return jwt.decode(token, current_app.config['JWT_SECRET'], algorithms=current_app.config['JWT_ALGORITHM'])
    except JWTError as e:
        six.raise_from(Unauthorized, e)


def _current_timestamp() -> int:
    return int(time.time())
