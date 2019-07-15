docker build . -t sflu.builder
docker run --rm -it -v ${PWD}:/app -w /app sflu.builder bash