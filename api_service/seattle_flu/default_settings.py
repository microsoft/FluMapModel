import os

TRUE_OPTIONS=["true", "1", "y", "yes", "on"]

# make sure DEBUG is off unless enabled explicitly otherwise
DEBUG = os.environ.get("DEBUG", "False").lower() in TRUE_OPTIONS or os.environ.get("FLASK_ENV", "production") == "development"
LOG_DIR = '.'  # create log files in current working directory

# This stores our results from a model request
REDIS_URI= os.environ.get('REDIS_URI', 'redis://redis:6379')
# This is our queuing engine
RABBIT_URI = os.environ.get('RABBIT_URI', 'amqp://guest:guest@rabbit:5672')

R_PATH = os.environ.get("R_PATH", '/usr/bin/Rscript')