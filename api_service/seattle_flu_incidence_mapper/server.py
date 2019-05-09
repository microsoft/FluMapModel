# local modules
from connexion.exceptions import OAuthProblem
from flask import request

import seattle_flu_incidence_mapper.config as config

# Get the application instance
app = config.connex_app
# Read the swagger.yml file to configure the endpoints
app.add_api("swagger.yml")

if __name__ == "__main__":
    app.run(debug=True)