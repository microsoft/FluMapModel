FROM python:3.7-stretch
RUN printf "deb http://cran.rstudio.com/bin/linux/debian stretch-cran34/" >> /etc/apt/sources.list.d/r.list && \ 
    apt-key adv --keyserver keyserver.ubuntu.com--recv-key 'E19F5F87128899B192B1A2C2AD5F960A256A04AF' && \
    apt update && apt install nginx r-base libssl-dev python-dev -y && \
    # Make project directory
    mkdir -p /app/seattle_flu

ADD docker_scripts/pip.conf /etc/pip.conf

ENV PYTHONPATH=/app:${PYTHONPATH}
ENV PATH=/app:${PATH}

WORKDIR /app

ADD setup.py README.rst /app/seattle_flu/
RUN cd seattle_flu && \
    pip3 install -e .[production]

# Add Model Related stuff
# Start with setup
ADD docker_scripts/setup_env.R  /app/
ADD models  /app/models
# Run our environment install which installs some
# basic R libs and then all models we have
RUN Rscript setup_env.R && \
    # And now our api
    pip install rse_api --upgrade

# Finish the setup
ADD docker_scripts/nginx.conf /etc/nginx/nginx.conf
ADD docker_scripts/entrypoint.sh docker_scripts/wsgi.ini ./
# Add the rest of our library here
ADD seattle_flu ./seattle_flu

CMD /app/entrypoint.sh