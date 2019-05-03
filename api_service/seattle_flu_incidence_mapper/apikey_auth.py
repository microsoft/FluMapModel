from connexion.exceptions import OAuthProblem

TOKEN_DB = {
    'asdf1234567890': {
        'uid': 100
    }
}

def apikey_auth(token, required_scopes):
    info = TOKEN_DB.get(token, None)

    if not info:
        raise OAuthProblem('Invalid token')

    return info