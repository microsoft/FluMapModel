#!/usr/bin/env sh
#
# We use this container during builds as a placeholder until Artifactory
# gets CRAN support

cd /tmp && \
    git clone https://github.com/UptakeOpenSource/cran-server && \
	cd cran-server && \
    docker build -t idm-docker-staging.packages.idmod.org/cran-server .
docker push idm-docker-staging.packages.idmod.org/cran-server