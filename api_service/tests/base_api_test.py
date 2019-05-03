import json
import os
import unittest
os.environ['DEBUG']='1'
from seattle_flu_incidence_mapper.server import connex_app

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


class BaseApiTest(unittest.TestCase):
    def setUp(self):
        self.app = connex_app.app.test_client()