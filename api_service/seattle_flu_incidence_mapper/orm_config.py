import functools
import os

from flask_marshmallow import Marshmallow
from flask_sqlalchemy import SQLAlchemy
from sqlalchemy import orm
from sqlalchemy.ext.declarative import declarative_base

db = None
session = None
base = None
ma = None

def get_declarative_base():
    global  base
    if base is None:
        base = declarative_base()
        base.query = get_session().query_property()
    return base


def get_db():
    return db


def get_session():
    return session


def setup_db(basedir, app):
    global db, session, ma

    # Configure the SQLAlchemy part of the app instance
    app.config['SQLALCHEMY_ECHO'] = True
    app.config['SQLALCHEMY_DATABASE_URI'] = os.getenv('SQLALCHEMY_DATABASE_URI', 'sqlite:////' + os.path.join(basedir, 'models.db'))
    app.config['SQLALCHEMY_TRACK_MODIFICATIONS'] = False
    Session = orm.sessionmaker()


    # Create the SQLAlchemy db instance
    db = SQLAlchemy(app)
    set_marshmallow(app)
    session = db.session
    return db


def get_marshmallow():
    return ma


def set_marshmallow(app):
    global ma
    # Initialize Marshmallow
    ma = Marshmallow(app)
    return ma