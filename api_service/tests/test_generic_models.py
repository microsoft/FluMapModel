import io
import json
import os

from tests.base_api_test import BaseApiTest, BASE_PATH


def read_file_and_format_for_upload(filepath):
    with open(os.path.join(BASE_PATH,filepath), 'rb') as fin:
        fil = io.BytesIO(fin.read())
    return os.path.join(BASE_PATH,filepath), fil, 'application/octet-stream'


url = '/v1/generic_models'
test_filehash = '0ee3faca1f6a66c65ae3d2ae67ab5939'

class TestUploadModel(BaseApiTest):

    def test_upload(self):
        headers = dict(
            Authorization="Bearer "
                          "eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpc3MiOiJzZWF0dGxlX2ZsdV9zdHVkeSIsImlhdCI6MTU1OTY5NDc"
                          "5MSwiZXhwIjoxNTkxMTQ0MzkxLCJzdWIiOiJ0ZXN0cyJ9.OL0NyX3iPm2zByCudPBB7yu0v1CO2wblsgW4BcgX7UY")
        test_data = {
            "name": 'test_model',
            "query_str": '{"observed":"x", "model_type": "smooth"}',
            "model": read_file_and_format_for_upload('{}.csv'.format(test_filehash)),
            "model_type": "smooth",
            "modelRDS": read_file_and_format_for_upload('{}.csv'.format(test_filehash)),
            "modelLatent": read_file_and_format_for_upload('{}.latent_field.csv'.format(test_filehash)),
        }

        response = self.app.post(url, headers=headers, data=test_data, mimetype='multipart/form-data')
        self.assertEqual(201, response.status_code,
                         "Create failed: {} - {} ".format(response.status_code, str(response.data)))

        model = json.loads(response.data)

        response = self.app.get(f"{url}/{model['id']}/model")
        self.assertEqual(200, response.status_code)

    def test_zlist(self):
        response = self.app.get(url)
        self.assertEqual(200, response.status_code)
        models = json.loads(response.data)
        self.assertIsInstance(models, list, 'Return is {} not list'.format(type(models)))