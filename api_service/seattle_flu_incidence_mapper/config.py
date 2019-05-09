import os
import connexion
from seattle_flu_incidence_mapper.orm_config import setup_db
from seattle_flu_incidence_mapper.utils import set_marshmallow

basedir = os.path.abspath(os.path.dirname(__file__))

# Create the Connexion application instance
connex_app = connexion.App("seattle_flu_incidence_mapper.config", specification_dir=os.path.join(basedir, 'swagger'))

# Get the underlying Flask app instance
app = connex_app.app
app.config['WORKER_IMAGE'] = os.environ.get('WORKER_IMAGE', 'idm-docker-production.packages.idmod.org/sfim-worker:latest')
app.config['MODEL_STORE'] = os.environ.get('MODEL_STORE', os.path.abspath(os.path.join(os.getcwd(), "../../test_model_store")))
app.config['MODEL_HOST_PATH'] = os.environ.get('MODEL_HOST_PATH',  os.path.abspath(os.path.join(os.getcwd(), "../../test_model_store")))

db = setup_db(basedir, app)
set_marshmallow(app)

# DO NOT MOVE this line. The order matters here
# we need to init our db before loading our models
from seattle_flu_incidence_mapper.models import *

if os.environ.get('DEBUG', '0').lower() in ['1', 'y', 'yes', 't', 'true']:
    db.create_all()

