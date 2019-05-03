import os

from flask import current_app


def save_model_file(file, name):
    basedir = current_app.config.get('MODEL_STORE', '/model_store')
    file.save(os.path.join(basedir, name))