Development Environment tips
============================

1. To run the API server locally you will need
    * Python 3
    * R
2. Setup pip to talk to IDM's public package repository by creating a pip.conf at ~/.pip/pip.conf on Unix and %APPDATA%\pip\pip.ini. For alternate locations for pip.conf/pip.ini see https://pip.pypa.io/en/stable/user_guide/#configuration. Set the contents of the file to::

    [global]
    index-url = https://packages.idmod.org/api/pypi/idm-python-production/simple

3. Checkout the code::

    $ git clone git@github.com:your_name_here/Seattle-Flu-Incidence-Mapper.git
4. Navigate to the checkout and run::

    $ pip install -r .[dev]
5. Run the `setup_env.R` script to install all the requisite R dependencies::

    $ Rscript setup_env.R

   On unix you may have to run the command above using *sudo*

6. You should now be able to execute app.py. You should run the following command within the seattle_flu folder to ensure the working directory is as expected for R::

    $ python3 app.py manage run

7. On Windows, you will need to set the path to your R executable. You can do this by setting the environment variable *R_PATH* to the path to Rscript.exe

UI/Model Develop Development Recommendation
=============================

If you are doing UI, Model Development, or development on an application that only talks to the API, running a docker container is the easist option

1. Install docker and docker-compose
2. Checkout the code::

    $ git clone git@github.com:your_name_here/Seattle-Flu-Incidence-Mapper.git

3. Navigate to the checkout and run::

    $ docker-compose build
4. Now run::
    $ docker-compose up -d
5. The server will be listening on port 5000 by default.
