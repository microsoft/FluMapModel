DOCKER_REPO=idm-docker-production.packages.idmod.org
APP_NAME=sfim

.PHONY: clean clean-test clean-pyc clean-build docs help
.DEFAULT_GOAL := help

BROWSER := python -c "$$BROWSER_PYSCRIPT"

help: ## This help.
	@awk 'BEGIN {FS = ":.*?## "} /^[a-zA-Z_-]+:.*?## / {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}' $(MAKEFILE_LIST)

clean: clean-build clean-pyc clean-test ## remove all build, test, coverage and Python artifacts

clean-build: ## remove build artifacts
	rm -fr build/
	rm -fr dist/
	rm -fr .eggs/
	find . -name '*.egg-info' -exec rm -fr {} +
	find . -name '*.egg' -exec rm -f {} +

clean-pyc: ## remove Python file artifacts
	find . -name '*.pyc' -exec rm -f {} +
	find . -name '*.pyo' -exec rm -f {} +
	find . -name '*~' -exec rm -f {} +
	find . -name '__pycache__' -exec rm -fr {} +

clean-test: ## remove test and coverage artifacts
	rm -f .coverage
	rm -fr htmlcov/
	rm -fr .pytest_cache

lint: ## check style with flake8
	flake8 --ignore=E501 rse_api tests

test-server: ## run tests quickly with the default Python
	PYTHONPATH=${PWD}:${PYTHONPATH} \
	py.test

coverage-server: ## check code coverage quickly with the default Python
	PYTHONPATH=${PWD}:${PYTHONPATH} \
	coverage run --source rse_api -m pytest
	coverage report -m
	coverage html
	$(BROWSER) htmlcov/index.html

docs-browsers: docs
	$(BROWSER) docs_src/_build/html/index.html

docs: ## generate Sphinx HTML documentation, including API docs
	rm -f docs_src/seattle_flu*.rst
	rm -f docs_src/modules.rst
	sphinx-apidoc -o docs_src/ seattle_flu_incidence_mapper
	$(MAKE) -C docs_src clean
	$(MAKE) -C docs_src html
	cp -r docs_src/_build/html/* docs
	# This disables githubs formatting on docs
	touch docs/.nojekyll

docs-release: docs
	git add docs


servedocs: docs ## compile the docs watching for changes
	watchmedo shell-command -p '*.rst' -c '$(MAKE) -C docs html' -R -D .



# DOCKER TASKS
# Build the container
build: ## Build the container
	docker build -t $(APP_NAME) .

build-nc: ## Build the container without caching
	docker build --no-cache -t $(APP_NAME) .


