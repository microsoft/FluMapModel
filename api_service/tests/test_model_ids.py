import unittest

from seattle_flu_incidence_mapper.utils import get_model_id


class TestGetIdFromQuery(unittest.TestCase):
    def test_ids_match_expected(self):
        ids = {
            "3224d863c7e0223d68a2962196098d84": {"observed":["catchment","encountered_week","flu_shot","n","pathogen",
                                                             "positive","PUMA5CE","sampling_location","time_row"],
                                                 "model_type":"smooth"},
            "fea4085ebb163309fc5417a05c0c1935": {"observed":["encountered_week","pathogen","PUMA5CE"],"model_type":"latent"}
        }

        for id, query_obj in ids.items():
            gen_id = get_model_id(query_obj)
            self.assertEqual(id, gen_id)
