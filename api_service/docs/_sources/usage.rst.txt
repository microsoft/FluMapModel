Usage
=====


Production Environment File:
============================
#. The following command will produce a production environment file for docker-compose containing a random
   password for postgres and random secret for JWT
   `export DB_PASS=$(</dev/urandom tr -dc 'A-Za-z0-9!"#%&'\''()*+,-./:;<=>?@[\]^_`{|}~' | head -c 32 ); printf "POSTGRES_PASS=%s\nJWT_SECRET=%s\nSQLALCHEMY_URI=postgres+psycopg2://seattle_flu:%s@db" $DB_PASS $(</dev/urandom tr -dc 'A-Za-z0-9!"#%&'\''()*+,-./:;<=>?@[\]^_`{|}~' | head -c 256 ) $DB_PASS > production.env`
# if you have a ramdsik, add that as the job_host_path
    printf "MODEL_HOST_PATH=/model_store\nWORKER_JOB_HOST_PATH=/mnt/ramdisk\nMODEL_JOB_PATH=/jobs" >> production.env
#. Run `docker-compose -f docker-compose.production.yml run service flask db upgrade`
#. Create users. Change user1, user2, etc below to list of users. Be sure to save the output
   ```
   for user in user1 user2
   do
      docker-compose -f docker-compose.production.yml run service flask generate-token $user
   done
   ```
#.

Migrations
==========



#. Run

Setup
=====

#. Generate credentials for postgres
#. Setup an environment file containing the following
#. Run
   `docker-compose -f docker-compose.production.yml service db init`
#. Run