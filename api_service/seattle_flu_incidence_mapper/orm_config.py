import functools
import os

from flask_sqlalchemy import SQLAlchemy
from sqlalchemy.ext.declarative import declarative_base

db = None
session = None
base = None


def get_declarative_base():
    global  base
    if base is None:
        base = declarative_base()
    return base


def get_db():
    return db


def get_session():
    return session


def setup_db(basedir, app):
    global db, session

    # Configure the SQLAlchemy part of the app instance
    app.config['SQLALCHEMY_ECHO'] = True
    app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('SQLALCHEMY_DATABASE_URI', 'sqlite:////' + os.path.join(basedir, 'models.db'))
    app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False


    # Create the SQLAlchemy db instance
    db = SQLAlchemy(app)
    session = db.session
    return db
