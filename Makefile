BUILD_CONTAINER_NAME ?= idm-docker-staging.packages.idmod.org/sfim-build-env:latest
DEPLOY_SERVER ?= 40.112.165.255
DEPOLY_USERNAME ?= useradmin

.PHONY= get_version build-r-env build-r-package build-api help

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

clean:
	find . -iname *.tar.gz -exec rm {} \;

get_version:
	$(eval version=$(git rev-parse --short HEAD))

build-r-env: ## build docker container that can package R scripts
	docker build -f Dockerfile.RBuildEnv . \
		-t $(BUILD_CONTAINER_NAME)

publish-r-env: build-r-env ## Publish our R Environment
	docker push $(BUILD_CONTAINER_NAME)

pull-r-env: # Pull our R Build environment
	docker pull $(BUILD_CONTAINER_NAME)

build-r-package: clean pull-r-env ## Build the r package as tar ball
    # as we add more models we can just build them all here
	# We have to run this with the build user's ids
	# otherwise we end up with files we cannot modify
	docker run -u $(shell id -u):$(shell id -g) \
		-v $(PWD)/:/app \
		-w /app $(BUILD_CONTAINER_NAME) \
		bash -c "cd dbViewR && R CMD build . && \
				cd ../incidenceMapR && R CMD build . && \
				cd ../modelServR && R CMD build ."

build-production: build-r-package get_version ## Builds the api
	-mkdir -p api_service/models
	docker-compose -f docker-compose.production.yml build

publish-production: build-production ## Publishes the API
	docker-compose -f docker-compose.production.yml push

deploy-api: ## Deploys over ssh to prod server. Requires setup of ssh before hand
	# setup should just be ssh-copy-id $(DEPOLY_USERNAME)@$(DEPLOY_SERVER)
	export DOCKER_HOST=ssh://$(DEPOLY_USERNAME)@$(DEPLOY_SERVER) && \
		docker-compose -f docker-compose.production.yml pull && \
		docker-compose -f docker-compose.production.yml up -d

