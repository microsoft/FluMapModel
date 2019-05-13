import os

from flask import current_app

from seattle_flu_incidence_mapper.models.generic_model import GenericModel


def create_id_from_query_str(query_str):
    return query_str


def get_model_id_from_query_str(query_str):
    # do query string cleanup here
    # TODO
    model = GenericModel.query.filter(GenericModel.query_str == query_str).first()
    return model


def get_model_file(id):
    basedir = current_app.config.get('MODEL_STORE', '/model_store')
    return os.path.join(basedir, f"{id}.csv")


def save_model_file(file, id):
    basedir = current_app.config.get('MODEL_STORE', '/model_store')
    file.save(os.path.join(basedir, id))