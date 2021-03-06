#!/bin/sh

echo ""
echo "========================================="
echo "Initializing CloudStore database..."
echo ""

psql -U postgres -f roles.sql
createdb -U cf_cloudstore cf_cloudstore
psql -U postgres -c 'create extension hstore;' cf_cloudstore
psql -U cf_cloudstore -f scheme.sql

echo ""
echo "DONE initializing"
echo ""
