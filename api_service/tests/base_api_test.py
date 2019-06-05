import json
import os
import unittest
from flask.testing import FlaskClient
from werkzeug.utils import cached_property
from werkzeug.wrappers import BaseResponse
os.environ['DEBUG']='1'
from seattle_flu_incidence_mapper.app import app

BASE_PATH = os.path.abspath(os.path.join(os.path.dirname(__file__), 'fixtures'))


def load_fixture(url):
    global BASE_PATH
    if url[0] == '/':
        url = url[1:]
    for ext in ['', '.json']:
        p = os.path.join(BASE_PATH, url + ext)
        if os.path.exists(p) and os.path.isfile(p):
            return json.load(open(p, 'r'))
    raise FileNotFoundError('Cannot find file {}'.format(p))


class Response(BaseResponse):
    @cached_property
    def json(self):
        return json.loads(self.data)


class TestClient(FlaskClient):
    def open(self, *args, **kwargs):
        if 'json' in kwargs:
            kwargs['data'] = json.dumps(kwargs.pop('json'))
            kwargs['content_type'] = 'application/json'
        return super(TestClient, self).open(*args, **kwargs)


class BaseApiTest(unittest.TestCase):
    def setUp(self):
        app.testing = True
        app.test_client_class = TestClient
        self.app = app.test_client()
