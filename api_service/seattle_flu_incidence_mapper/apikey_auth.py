from typing import Dict

from connexion.exceptions import OAuthProblem
import json
import os
import functools

DEFAULT_TOKEN_DB_PATH = os.environ.get("API_TOKEN_DB", "/model_tokens")


@functools.lru_cache(maxsize=1)
def get_token_db() -> Dict:
    """
    Load our token db from json file

    we cache this function with lru_cache an no arguments so effectively it is a singleton
    :return: Token dict
    """
    if os.environ.get('DEBUG', '0').lower() in ['1', 'y', 'yes', 't', 'true']:
        return { 'sample_token': {'user': 'abc', 'role':'admin'}}
    return json.load(open(os.path.join(DEFAULT_TOKEN_DB_PATH, "tokens.json"), "r"))


def apikey_auth(token: str, required_scopes):
    info = get_token_db.get(token, None)

    if not info:
        raise OAuthProblem('Invalid token')

    return info