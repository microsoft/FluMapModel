from tests.base_api_test import BaseApiTest

url = '/v1/query'
test_filehash = '0ee3faca1f6a66c65ae3d2ae67ab5939'

class TestUploadModel(BaseApiTest):

    def test_query(self):
        json_data = {'yay': 'yay'}
        response = self.app.post(url, json=json_data)
        self.assertEqual(200, response.status_code)