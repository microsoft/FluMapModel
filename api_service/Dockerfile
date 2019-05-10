FROM python:3.7-stretch
RUN apt update && apt install nginx \
        python-dev \
        apt-transport-https \
        ca-certificates \
        curl \
        gnupg2 \
        software-properties-common \
        -y && \
    curl -fsSL https://download.docker.com/linux/$(. /etc/os-release; echo "$ID")/gpg > /tmp/dkey; apt-key add /tmp/dkey && \
        add-apt-repository \
        "deb [arch=amd64] https://download.docker.com/linux/debian \
        $(lsb_release -cs) \
        stable" && \
        apt-get update && \
        apt-get install docker-ce docker-ce-cli containerd.io -y
    # Make project directory
RUN mkdir -p /app/seattle_flu_incidence_mapper \
        /model_store
ADD docker_scripts/pip.conf /etc/pip.conf

ENV PYTHONPATH=/app:${PYTHONPATH}
ENV PATH=/app:${PATH}

WORKDIR /app

ENV MODEL_STORE=/model_store

ADD setup.py README.rst /app/seattle_flu/
RUN cd seattle_flu && \
    pip3 install -e .[production]

# Define we will have a model data folder
VOLUME /model_data
# Finish the setup
ADD docker_scripts/nginx.conf /etc/nginx/nginx.conf
ADD docker_scripts/entrypoint.sh docker_scripts/wsgi.ini ./
# Add the rest of our library here
ADD seattle_flu_incidence_mapper ./seattle_flu_incidence_mapper

EXPOSE 80
VOLUME [/model_store]

CMD /app/entrypoint.sh