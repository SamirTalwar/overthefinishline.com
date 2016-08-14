#!/bin/bash

set -e

psql -v ON_ERROR_STOP=1 --username "$POSTGRES_USER" <<-EOSQL
    CREATE USER dashboard WITH PASSWORD '$DATABASE_DASHBOARD_PASSWORD';
    CREATE DATABASE dashboard;
    GRANT ALL PRIVILEGES ON DATABASE dashboard TO dashboard;
EOSQL
