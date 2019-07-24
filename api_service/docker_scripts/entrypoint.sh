#!/bin/bash


if [ "$DEBUG" = "1" ]; then
    export FLASK_DEV=1
    export FLASK_ENV=development
    echo "Running Flask Debug Server"
    export PYTHONPATH=/app
    cd /app
    flask run --host=0.0.0.0
else
    echo "Launching Nginx"
    cd /app
    uwsgi --ini /app/wsgi.ini --wsgi-disable-file-wrapper &
    nginx
    tail -f /var/log/nginx/*
fi