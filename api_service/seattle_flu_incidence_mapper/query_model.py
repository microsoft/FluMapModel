# API Methods for the /query
import tarfile
import uuid
from io import BytesIO
import docker
from flask import current_app
from sqlalchemy.orm.exc import NoResultFound

from seattle_flu_incidence_mapper.models.pathogen_model import PathogenModel
from seattle_flu_incidence_mapper.utils import get_model_id

loaded_models = []
client = docker.DockerClient()
api_client = docker.APIClient()


def query(query_json):
    model_id = get_model_id(query_json)
    try:
        model = PathogenModel.query.filter(PathogenModel.id == model_id).order_by(PathogenModel.created.desc()).first()
        s = None
        try:
            container = client.containers.get(f'sfim-{model_id}')
        except docker.errors.NotFound:
            container = None
        # start container is not running
        if container is None:
            image = current_app.config['WORKER_IMAGE']
            container = client.containers.run(image, name=f"sfim-{model_id}", tty=True, detach=True, stdin_open=True, )
            s = container.attach_socket(params={'stdin': 1, 'stream': 1})
            # initialize our model by loading
            s._sock.send(f'library(modelServR)\nmodel < - loadModelFileById("{model_id}")\n'.encode('utf-8'))

        # if we need to connect to an existing container, do do now
        if s is None:
            s = container.attach_socket(params={'stdin': 1, 'stream': 1})

        # define where we want our output written too
        outfile = str(uuid.uuid4())

        # Run our query against the model(should already be loaded)
        command = f'queryLoadedModel(model, "{outfile}")\n'
        s._sock.send(command.encode('utf-8'))
        s.close()

        # Fetch our result
        file_json = container.get_archive(f'/tmp/{outfile}')

        # Fetch data from stream
        # TODO , in prod maybe stream to user?
        stream, stat = file_json
        file_obj = BytesIO()
        for i in stream:
            file_obj.write(i)
        file_obj.seek(0)
        tar = tarfile.open(mode='r', fileobj=file_obj)
        text = tar.extractfile(outfile)
        return text
    except NoResultFound:
        raise NoResultFound




