docker build . -t sflu.builder
docker run --rm -it -v //var/docker/docker.sock::/var/run/docker.sock -v ${PWD}:/app -w /app sflu.builder bash