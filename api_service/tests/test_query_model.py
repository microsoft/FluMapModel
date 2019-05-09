from tests.base_api_test import BaseApiTest

url = '/v1/query'
test_filehash = '0ee3faca1f6a66c65ae3d2ae67ab5939'

class TestUploadModel(BaseApiTest):

    def test_query(self):
        json_data = {"observed":["catchment","encountered_week","flu_shot","n","pathogen",
                                                             "positive","PUMA5CE","sampling_location","time_row"],
                                                 "model_type":"smooth"}
        response = self.app.post(url, json=json_data)
        self.assertEqual(200, response.status_code)