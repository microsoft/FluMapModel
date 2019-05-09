FROM python:3.7-stretch
RUN apt update && apt install nginx python-dev -y && \
    # Make project directory
    mkdir -p /app/seattle_flu_incidence_mapper

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


VOLUME [/model_store]

CMD /app/entrypoint.sh