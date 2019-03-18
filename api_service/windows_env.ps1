docker build . -t rse_api.builder
docker run --rm -it -v ${PWD}:/app -w /app rse_api.builder bash