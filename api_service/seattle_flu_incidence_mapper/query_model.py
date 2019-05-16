# API Methods for the /query
import hashlib
import os
import tarfile
import time
from io import BytesIO
import docker
from flask import current_app, send_file, request
from sqlalchemy.orm.exc import NoResultFound
import json
from seattle_flu_incidence_mapper.models.generic_model import GenericModel
from seattle_flu_incidence_mapper.utils import get_model_id, ModelExecutionException

loaded_models = []
client = docker.DockerClient()
api_client = docker.APIClient()


def query(query_json):
    file_format  ='csv' if 'csv' in request.headers.get('accept', 'json').lower() else 'json'
    created = False
    container = None
    try:

        model_id = get_model_id(query_json)
        host_job_path = os.path.join(current_app.config['WORKER_JOB_HOST_PATH'], model_id)
        job_path = os.path.join(current_app.config['MODEL_JOB_PATH'], model_id)
        model: GenericModel = GenericModel.query.filter(GenericModel.id == model_id).order_by(GenericModel.created.desc()).first()

        if model is None:
            raise NoResultFound(f"Could not find the model with the id {model_id} from query string: {json.dumps(query_json)}")


        # define where we want our output written too
        # let's cache the users query and model to reduce calls to R. This could change
        # for future models who have more interactive stochastic outputs
        outfile = hashlib.md5(json.dumps(dict(id=model_id,
                                              created=str(model.created),
                                              file_format=file_format)).encode('ascii')).hexdigest()
        full_outpath = os.path.join(job_path, outfile)
        if not os.path.exists(full_outpath):
            os.makedirs(job_path)
            # new request or a updated model.
            # We have our model, lets check to see if we alread have a worker container
            container, socket, created = get_or_create_model_container(job_path, host_job_path, model_id)
            execute_model_query(socket, file_format, outfile)

        lock_path = full_outpath + ".lock"
        time.sleep(0.1)
        x = 0
        while os.path.exists(lock_path) and x < 10:
            time.sleep(0.05)
            x += 1
        return send_file(
            full_outpath,
            as_attachment=False,
            mimetype='application/json' if file_format == 'json' else 'text/csv'
        )
    # Rethrow error for 404s
    except NoResultFound as e:
        raise e
    except Exception as e:
        current_app.logger.exception(e)
        if created and container:
            try:
                container.stop()
            except:
                pass


def execute_model_query(socket, file_format, outfile):
    """

    :param file_format:
    :param outfile:
    :param socket:
    :return:
    """
    # Run our query against the model(should already be loaded)
    command = f'queryLoadedModel(model, "{outfile}", format="{file_format}")\n'
    socket._sock.send(command.encode('utf-8'))
    socket.close()
    time.sleep(0.05)


def get_or_create_model_container(local_job_path, host_job_path, model_id):
    """

    :param job_path:
    :param model_id:
    :return:
    """
    socket = None
    created = False
    try:

        container = client.containers.get(f'sfim-{model_id}')
    except docker.errors.NotFound:
        container = None
    # start container if it is not running
    if container is None:
        created = True
        image = current_app.config['WORKER_IMAGE']
        container_volumes = {
            current_app.config['MODEL_HOST_PATH']: {
                'bind': '/worker_model_store',
                'mode': 'ro'
            },
            current_app.config['WORKER_JOB_HOST_PATH']: {
                'bind': '/jobs',
                'mode': 'rw'
            }
        }
        container_env = dict(MODEL_STORE="/worker_model_store",
                             WORKER_DIR=f"/jobs/{model_id}")
        container = client.containers.run(image,
                                          name=f"sfim-{model_id}",
                                          tty=True, detach=True,
                                          environment=container_env,
                                          volumes=container_volumes,
                                          stdin_open=True,
                                          auto_remove=True)
        socket = container.attach_socket(params={'stdin': 1, 'stream': 1})
        # initialize our model by loading
        socket._sock.send(f'library(modelServR)\nmodel <- loadModelFileById("{model_id}")\n'.encode('utf-8'))
        time.sleep(0.1)
    # if we need to connect to an existing container, do do now
    if socket is None:
        socket = container.attach_socket(params={'stdin': 1, 'stream': 1})
    return container, socket, created




