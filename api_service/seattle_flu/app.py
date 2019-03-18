
import os

from seattle_flu.lib import get_app
from seattle_flu.lib.common import dynamic_import_all

dir_path = os.path.dirname(os.path.realpath(__file__))

# Initialize our Flask application. We provide the path to our default settings object
# If you do not need dramtiq, use the following instead
# application = get_application(SETTING_OBJECT, setup_broker_func=None)
application = get_app()

# setup controllers
dynamic_import_all('seattle_flu.controllers')
dynamic_import_all('seattle_flu.tasks')

# Run the cli and let the parameters determine what we
# To run, run python app.py manage run
# or to setup db run
# python app.py db create
if __name__ == "__main__":
    application.cli()
