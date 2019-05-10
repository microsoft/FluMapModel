

from setuptools import setup, find_packages
from os import path
here = path.abspath(path.dirname(__file__))

# Get the long description from the README file
with open(path.join(here, 'README.rst')) as f:
    long_description = f.read()

setup(
    name='seattle_flu_incidence_mapper',  # Required

    # Versions should comply with PEP 440:
    # https://www.python.org/dev/peps/pep-0440/
    version='1.0.0',  # Required
    description='Seattle Flu Incidence Mapper List',
    long_description=long_description,
    long_description_content_type='text/markdown',
    url='https://github.com/InstituteforDiseaseModeling/seattle_flu',
    author='Institute for Disease Modeling, Mike Famulare, Clinton Collins',
    author_email='mfamulare@idmod.org, ccollins@idmod.org',
    # Classifiers help users find your project by categorizing it.
    #
    # For a list of valid classifiers, see https://pypi.org/classifiers/
    classifiers=[  # Optional
        # How mature is this project? Common values are
        #   3 - Alpha
        #   4 - Beta
        #   5 - Production/Stable
        'Development Status :: 4 - Beta',

        # Indicate who your project is intended for
        # https://pypi.org/pypi?%3Aaction=list_classifiers
        'Intended Audience :: Science/Research',

        # Pick your license as you wish
        'License :: OSI Approved :: MIT License',
        'Programming Language :: Python :: 3.6',
    ],

    keywords='seattle_flu epidemiology, flu',
    packages=find_packages(exclude=['contrib', 'docs', 'tests']),  # Required

    # For an analysis of "install_requires" vs pip's requirements files see:
    # https://packaging.python.org/en/latest/requirements.html
    install_requires=[
        'connexion[swagger-ui]',
        'flask_sqlalchemy',
        'flask_marshmallow',
        'marshmallow-sqlalchemy',
        "docker",
        'psycopg2-binary',
        ''
    ],

    extras_require={
        'dev': ['check-manifest', 'nose', 'sphinx', 'sphinxcontrib-plantuml'],
        'tests': ['coverage'],
        'upload': ['requests', 'tqdm'],
        'production': ['uwsgi']
    },
    project_urls={
        'Bug Reports': 'https://github.com/InstituteforDiseaseModeling/Seattle-Flu-Incidence-Mapper/issues',
        'Source': 'https://github.com/InstituteforDiseaseModeling/Seattle-Flu-Incidence-Mapper',
    }
)
