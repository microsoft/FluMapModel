from connexion.exceptions import OAuthProblem
import json
import os
import functools

DEFAULT_TOKEN_DB_PATH = os.environ("API_TOKEN_DB", "/model_tokens")


@functools.lru_cache
def get_token_db():
    """
    Load our token db from json file

    we cache this function with lru_cache an no arguments so effectively it is a singleton
    """
    if os.environ.get('DEBUG', '0').lower() in ['1', 'y', 'yes', 't', 'true']:
        return { 'sample_token': {'user': 'abc', 'role':'admin'}}
    return json.load(open(os.path.join(DEFAULT_TOKEN_DB_PATH, "tokens.json"), "r"))


def apikey_auth(token, required_scopes):
    info = get_token_db.get(token, None)

    if not info:
        raise OAuthProblem('Invalid token')

    return info