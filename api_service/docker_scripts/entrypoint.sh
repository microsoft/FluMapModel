#!/bin/bash

if [ "$DEBUG" = "1" ]; then
    echo "Running Flask Debug Server"
    export PYTHONPATH=/app
    cd /app
    python seattle_flu/app.py manage run --host=0.0.0.0
else
    echo "Launching Nginx"
    cd /app
    uwsgi --ini /app/wsgi.ini --wsgi-disable-file-wrapper &
    nginx
    tail -f /var/log/nginx/*
fi