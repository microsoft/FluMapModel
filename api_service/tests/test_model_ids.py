import unittest

from seattle_flu_incidence_mapper.utils import get_model_id


class TestGetIdFromQuery(unittest.TestCase):
    def test_ids_match_expected(self):
        ids = {
            "46e66b1a0faa59e12e53fb4166b881ca": {"model_type":"inla","observed":["catchment","encountered_week","flu_shot","n","pathogen","positive","PUMA5CE","sampling_location","time_row"],"pathogen":["vic"]},
            "29cc23488ba96c938113852c28b55c13": {"model_type":"inla latent","observed":["encountered_week","pathogen","PUMA5CE"],"pathogen":["vic"]}
        }

        for id, query_obj in ids.items():
            gen_id = get_model_id(query_obj)
            self.assertEqual(id, gen_id)
