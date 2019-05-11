import os

from flask_sqlalchemy import SQLAlchemy

db = None

def get_db():
    return db


def setup_db(basedir, app):
    global  db

    # Configure the SQLAlchemy part of the app instance
    app.config['SQLALCHEMY_ECHO'] = True
    app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('SQLALCHEMY_DATABASE_URI','sqlite:////' + os.path.join(basedir, 'models.db'))
    app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False


    # Create the SQLAlchemy db instance
    db = SQLAlchemy(app)
    return db