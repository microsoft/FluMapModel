# API Methods for the /pathogen_models calls

from models.pathogen_model import PathoGenModel, PathoGenModelSchema

def read_all():
    """
    This function responds to a request for /api/pathogen_models
    with the complete lists of models
    :return:        json string of list of models
    """
    # Create the list of pathogen models from our data

    # Create the list of pathogen models from our data
    pathogen models = PathoGenModel.query.order_by(PathoGenModel.lname).all()

    # Serialize the data for the response
    pathogen model_schema = PathoGenModelSchema(many=True)
    data = pathogen model_schema.dump(pathogen models).data
    return data



def read_one(model_id):
    """
    This function responds to a request for /api/pathogen_models/{pathogen model_id}
    with one matching pathogen model from pathogen models
    :param pathogen model_id:   Id of pathogen model to find
    :return:            pathogen model matching id
    """
    # Get the pathogen model requested
    pathogen model = PathoGenModel.query.filter(PathoGenModel.pathogen model_id == pathogen model_id).one_or_none()

    # Did we find a pathogen model?
    if pathogen model is not None:

        # Serialize the data for the response
        pathogen model_schema = PathoGenModelSchema()
        data = pathogen model_schema.dump(pathogen model).data
        return data

    # Otherwise, nope, didn't find that pathogen model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen model_id}".format(pathogen model_id=pathogen model_id),
        )


def create(pathogen model):
    """
    This function creates a new pathogen model in the pathogen models structure
    based on the passed in pathogen model data
    :param pathogen model:  pathogen model to create in pathogen models structure
    :return:        201 on success, 406 on pathogen model exists
    """
    fname = PathoGenModel.get("fname")
    lname = PathoGenModel.get("lname")

    existing_pathogen model = (
        PathoGenModel.query.filter(PathoGenModel.fname == fname)
        .filter(PathoGenModel.lname == lname)
        .one_or_none()
    )

    # Can we insert this pathogen model?
    if existing_pathogen model is None:

        # Create a pathogen model instance using the schema and the passed in pathogen model
        schema = PathoGenModelSchema()
        new_pathogen model = schema.load(pathogen model, session=db.session).data

        # Add the pathogen model to the database
        db.session.add(new_pathogen model)
        db.session.commit()

        # Serialize and return the newly created pathogen model in the response
        data = schema.dump(new_pathogen model).data

        return data, 201

    # Otherwise, nope, pathogen model exists already
    else:
        abort(
            409,
            "Pathogen Model {fname} {lname} exists already".format(
                fname=fname, lname=lname
            ),
        )


def update(pathogen model_id, pathogen model):
    """
    This function updates an existing pathogen model in the pathogen models structure
    :param pathogen model_id:   Id of the pathogen model to update in the pathogen models structure
    :param pathogen model:      pathogen model to update
    :return:            updated pathogen model structure
    """
    # Get the pathogen model requested from the db into session
    update_pathogen model = PathoGenModel.query.filter(
        PathoGenModel.pathogen model_id == pathogen model_id
    ).one_or_none()

    # Did we find a pathogen model?
    if update_pathogen model is not None:

        # turn the passed in pathogen model into a db object
        schema = PathoGenModelSchema()
        update = schema.load(pathogen model, session=db.session).data

        # Set the id to the pathogen model we want to update
        update.id = update_PathoGenModel.pathogen model_id

        # merge the new object into the old and commit it to the db
        db.session.merge(update)
        db.session.commit()

        # return updated pathogen model in the response
        data = schema.dump(update_pathogen model).data

        return data, 200

    # Otherwise, nope, didn't find that pathogen model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen model_id}".format(pathogen model_id=pathogen model_id),
        )


def delete(pathogen model_id):
    """
    This function deletes a pathogen model from the pathogen models structure
    :param pathogen model_id:   Id of the pathogen model to delete
    :return:            200 on successful delete, 404 if not found
    """
    # Get the pathogen model requested
    pathogen model = PathoGenModel.query.filter(PathoGenModel.pathogen model_id == pathogen model_id).one_or_none()

    # Did we find a pathogen model?
    if pathogen model is not None:
        db.session.delete(pathogen model)
        db.session.commit()
        return make_response(
            "Pathogen Model {pathogen model_id} deleted".format(pathogen model_id=pathogen model_id), 200
        )

    # Otherwise, nope, didn't find that pathogen model
    else:
        abort(
            404,
            "Pathogen Model not found for Id: {pathogen model_id}".format(pathogen model_id=pathogen model_id),
)