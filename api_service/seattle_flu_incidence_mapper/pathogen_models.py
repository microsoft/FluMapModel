# API Methods for the /pathogen_models calls
from seattle_flu_incidence_mapper.model_store import save_model_file, get_model_file
from seattle_flu_incidence_mapper.models.pathogen_model import PathogenModel, PathogenModelSchema
from seattle_flu_incidence_mapper.config import db
from flask import abort, request, config, current_app, make_response, send_file


def read_all():
    """
    This function responds to a request for /api/pathogen_models
    with the complete lists of models
    :return:        json string of list of models
    """
    # Create the list of pathogen_models from our data

    # Create the list of pathogen_models from our data
    pathogen_models = PathogenModel.query.order_by(PathogenModel.created.desc()).all()

    # Serialize the data for the response
    pathogen_model_schema = PathogenModelSchema(many=True)
    data = pathogen_model_schema.dump(pathogen_models).data
    return data


def read(model_id):
    """
    This function responds to a request for /api/pathogen_models/{pathogen_model_id}
    with one matching pathogen_model from pathogen_models
    :param pathogen_model_id:   Id of pathogen_model to find
    :return:            pathogen_model matching id
    """
    # Get the pathogen_model requested
    pathogen_model = PathogenModel.query.filter(PathogenModel.id == model_id).one_or_none()

    # Did we find a pathogen_model?
    if pathogen_model is not None:

        # Serialize the data for the response
        pathogen_model_schema = PathogenModelSchema()
        data = pathogen_model_schema.dump(pathogen_model).data
        return data

    # Otherwise, nope, didn't find that pathogen_model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {id}".format(id=model_id),
        )


def make_id_from_query_str(param):
    return "aaaaa"


def create():
    """
    This function creates a new pathogen_model in the pathogen_models structure
    based on the passed in pathogen_model data
    :param pathogen_model:  pathogen_model to create in pathogen_models structure
    :return:        201 on success, 406 on pathogen_model exists
    """


    #build our pathogenmodel object first
    pathogen_model = dict(id=make_id_from_query_str(request.form['query_str']),
                          name=request.form['name'],
                          query_str=request.form['query_str'],
                          latent='modelLatent' in request.files)

    schema = PathogenModelSchema()
    new_pathogen_model = schema.load(pathogen_model, session=db.session).data
    # Add the pathogen_model to the database
    db.session.add(new_pathogen_model)
    db.session.commit()

    # save the files to our config directory
    save_model_file(request.files['model'], f'{new_pathogen_model.id}.csv')
    save_model_file(request.files['modelRDS'], f'{new_pathogen_model.id}.RDS')
    if 'modelLatent' in request.files:
        save_model_file(request.files['modelLatent'], f'{new_pathogen_model.id}.latent_field.csv')
    # Serialize and return the newly created pathogen_model in the response
    data = schema.dump(new_pathogen_model).data

    return data, 201


def update(pathogen_model_id, pathogen_model):
    """
    This function updates an existing pathogen_model in the pathogen_models structure
    :param pathogen_model_id:   Id of the pathogen_model to update in the pathogen_models structure
    :param pathogen_model:      pathogen_model to update
    :return:            updated pathogen_model structure
    """
    # Get the pathogen_model requested from the db into session
    update_pathogen_model = PathogenModel.query.filter(
        PathogenModel.id == pathogen_model_id
    ).one_or_none()

    # Did we find a pathogen_model?
    if update_pathogen_model is not None:

        # turn the passed in pathogen_model into a db object
        schema = PathogenModelSchema()
        update = schema.load(pathogen_model, session=db.session).data

        # Set the id to the pathogen_model we want to update
        update.id = update_pathogen_model.id

        # merge the new object into the old and commit it to the db
        db.session.merge(update)
        db.session.commit()

        # return updated pathogen_model in the response
        data = schema.dump(update_pathogen_model).data

        return data, 200

    # Otherwise, nope, didn't find that pathogen_model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen_model_id}".format(pathogen_model_id=pathogen_model_id),
        )


def delete(pathogen_model_id):
    """
    This function deletes a pathogen_model from the pathogen_models structure
    :param pathogen_model_id:   Id of the pathogen_model to delete
    :return:            200 on successful delete, 404 if not found
    """
    # Get the pathogen_model requested
    pathogen_model = PathogenModel.query.filter(PathogenModel.id == pathogen_model_id).one_or_none()

    # Did we find a pathogen_model?
    if pathogen_model is not None:
        db.session.delete(pathogen_model)
        db.session.commit()
        return make_response(
            "Pathogen Model {pathogen_model_id} deleted".format(pathogen_model_id=pathogen_model_id), 200
        )

    # Otherwise, nope, didn't find that pathogen_model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen_model_id}".format(pathogen_model_id=pathogen_model_id),
)


def model_file(modelId):
    """
    Returns the model file to the user
    :param pathogen_model_id:
    :return:
    """
    # Get the pathogen_model requested from the db into session
    pathogen_model = PathogenModel.query.filter(
        PathogenModel.id == modelId
    ).order_by(PathogenModel.created.desc()).first()

    # Did we find a pathogen_model?
    if pathogen_model is not None:
        is_latent = request.args.get("latent", "0").lower() in ('1', 'y', 'yes', 't', 'true', True, 1)
        send_file(get_model_file(modelId,latent=is_latent))
    # Otherwise, nope, didn't find that pathogen_model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen_model_id}".format(pathogen_model_id=pathogen_model_id),
        )


def model_rds(modelId):
    """
        Returns the model file to the user
        :param pathogen_model_id:
        :return:
        """
    # Get the pathogen_model requested from the db into session
    pathogen_model = PathogenModel.query.filter(
        PathogenModel.id == modelId
    ).one_or_none()

    # Did we find a pathogen_model?
    if pathogen_model is not None:
        send_file(get_model_file(modelId, rds=True))
    # Otherwise, nope, didn't find that pathogen_model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen_model_id}".format(pathogen_model_id=pathogen_model_id),
        )