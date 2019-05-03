# API Methods for the /query
import docker
import dockerpty

loaded_models = []
client = docker.DockerClient()
api_client = docker.APIClient()

#image = 'sfli.modelServe'
image="r-base"


def query(query_json):
    try:
        container = client.containers.get('sfim-aaaa-1')
    except docker.errors.NotFound:
        container = None
    if container is None: # need to check container is running
        #start the model worker
        container = client.containers.run(image, name="sfim-aaaa-1", tty=True, detach=True, stdin_open=True,)


    command = "2 + 2\n"
    s = container.attach_socket(params={'stdin': 1, 'stream': 1})
    s._sock.send(command.encode('utf-8'))
    s.close()
    # now query the container

    s = container.logs(stdout=True, stderr=False, tail=1)
    return s

