import os
from flask import Flask
from flask_restful import Api
from seattle_flu.lib.common import singleton_function
from seattle_flu.lib.error import register_error_handlers
from seattle_flu.lib.tasks import get_worker_cli
from seattle_flu.lib.tasks import get_broker
from seattle_flu.lib.tasks import default_dramatiq_setup_result_backend


@singleton_function
def get_app():
    app = Flask(__name__)
    app.config.from_object('seattle_flu.default_settings')
    if 'SERVICE_SETTINGS' in os.environ:
        app.config.from_envvar('SERVICE_SETTINGS')
        app.url_map.strict_slashes = False
    register_error_handlers(app)

    get_worker_cli(app)
    app.api = Api(app)
    app.broker = get_broker(app)
    default_dramatiq_setup_result_backend(app, app.broker)
    return app






