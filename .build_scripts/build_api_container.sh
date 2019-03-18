#!/usr/bin/env sh
#
# This builds the Docker Container we use to create R packages for our Flu Models
# This environment is quite complex and is annoying enough to warrant a special
# docker environment.

# This scripts is expected to be execute by bamboo from the main directory
# of the git checkout

make publish-api