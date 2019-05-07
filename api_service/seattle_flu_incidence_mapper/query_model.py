# API Methods for the /query
import docker
from flask import current_app
from seattle_flu_incidence_mapper.model_store import get_model_id_from_quetry_str

loaded_models = []
client = docker.DockerClient()
api_client = docker.APIClient()


def query(query_json):
    model = get_model_id_from_quetry_str(query_json)
    if model:
        try:
            container = client.containers.get(f'sfim-{model.id}-1')
        except docker.errors.NotFound:
            container = None
        if container is None: # need to check container is running
            #start the model worker
            image = current_app.config['WORKER_IMAGE']
            container = client.containers.run(image, name=f"sfim-{model.id}", tty=True, detach=True, stdin_open=True,)


        command = "2 + 2\n"
        s = container.attach_socket(params={'stdin': 1, 'stream': 1})
        s._sock.send(command.encode('utf-8'))
        s.close()
        # now query the container

        s = container.logs(stdout=True, stderr=False, tail=1)
        return s

