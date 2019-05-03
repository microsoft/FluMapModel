import os

from flask import current_app


def get_model_file(id, rds:bool = False, latent:bool = False):
    basedir = current_app.config.get('MODEL_STORE', '/model_store')
    if rds:
        return os.path.join(basedir, f"{id}.RDS")
    elif latent:
        return os.path.join(basedir, f"{id}.latent_field.cvs")
    else:
        return os.path.join(basedir, f"{id}.csv")


def save_model_file(file, id):
    basedir = current_app.config.get('MODEL_STORE', '/model_store')
    file.save(os.path.join(basedir, id))