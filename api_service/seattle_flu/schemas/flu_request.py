from marshmallow import Schema
from marshmallow.fields import List, String
from marshmallow.validate import OneOf


# This defines our model query schema. This will be ingested by API and passed to the model to execute
class ModelQuery(Schema):
    output = List(String(validate=OneOf(["incidence"])), default=['incidence'])
    attributes = List(String(validate=OneOf(["Census_Tract", "vax_status"]), default=["Census_Tract","vax_status"]))
    basis = List(String(validate=OneOf(["year"])), default=["year"])
    values = List(String(validate=OneOf(['incidence_median','incidence_sd'])),
                  default=['incidence_median','incidence_sd'])
