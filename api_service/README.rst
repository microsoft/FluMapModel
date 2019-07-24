Seattle Flu Incidence Mapper API
================================

Seattle Flu Incidence Mapper API service hosts and API that can be used to host models generated using the incidence_mapper
R packages. These packages must best uploaded to the API server using the upload_models.py script


Architecture
------------

The API service is structured as 2 sets of docker images. The main image is the sflu-api which runs the API and
orchestrates the worker packages. The worker container use the sfim-worker image. The sflu-api is used to create the
service container.

The API service store model meta-information using a Postgres Database.

When a request is made to the /query API, the corresponding model is found in the database. If found, the service
container will then check is a copy of the selected model is already loaded into a running container. If not, service
container will create a new worker container using the sflu-worker image and then pass the request on to the model.


Development
===========

You will first need to build the docker image. To do this, ensure docker is installed. Then run the following commands
```
docker-compose build
```

You can now run the server using
```
docker-compose up
```

You should should always stop the containers completely and remove for proper development environment behaviour using
```
docker-compose down
```


Known Issues
------------
The container currently will create some root folders in development mode. This will be resolved in the next release

Performing Migrations
---------------------

docker-compose run service flask migrate

SQLALCHEMY_DATABASE_URI=postgres+psycopg2://seattle_flu:seattle_flu@db MODEL_STORE=/home/clinton/development/work/incidence-mapper/api_service/dbconversion/model_store FLASK_APP=seattle_flu_incidence_mapper.app:app flask db --help