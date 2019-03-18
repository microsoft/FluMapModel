from flask import request
from flask_restful import Resource
from seattle_flu.tasks.r_flu_model import r_flu_model_request
from rse_api.decorators import register_resource

from seattle_flu.app import application


@register_resource(['/flu', '/flu/<string:model_id>'])
class FluModelController(Resource):
    def post(self, model_id = None):
        """
        This is currently our only API endpoint.

        Currently the inputs can be one ot the two possible inputs are as follows

        ``
        {
            "output":["incidence"],
            "attributes":["Census_Tract","vax_status"],
            "basis":["year"],
            "values":["incidence_median","incidence_sd"]
        }
        ``

        or
        ``
        {

          "output": ["observed"],
          "attributes": ["Census_Tract", "vax_status"],
          "basis": ["year"],
          "values": ["flu_count", "nsamples"]

        }
        ``

        Args:
            model_id: Id of model to load
        Notes:
            Some questions to answer
            - Is this the best format to use? Could we possibly change to get with all the parameters in the URL?
            - Is there a better endpoint name other than flu?


        Returns:

        """
        #TODO validate this data using our schema
        body = request.data.decode('utf-8').strip()

        # Currently multiple options to explore to keep these R models in memory
        # 1. Worker server, If we do this, might as well make it a direct R server
        # 2. r2py - We could do an init here in the controller and load multiple models
        #   as users make requests. ie
        # def __init__():
        #    self.models = {}
        # def load_model(model_id):
        #   self.models[model_id] = r2py(model)
        # 3. Same as 2 but use subprocess to produce interactive sessions
        # 4. Queue based worker. This would probably introduce latency unless we used push based queues instead of
        # polling. Worth investigating as this would  allow more independent scaling from API and the models. This
        # makes sense since models will be the one that eats the resources and be the bottleneck so having them
        # scale independently would use resources the wisest.

        # Once we support multiple versions of models, we will have a grab latest model logic here
        # For now, lets just hard code it
        if model_id is None:
            model_id = 1
        message = r_flu_model_request.send(model_id, body)
        response = application.response_class(
            response=message.get_result(block=True),
            status=200,
            mimetype='application/json'
        )
        return response
