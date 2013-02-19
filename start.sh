#!/usr/bin/env bash

while getopts "d" option; do
  case "${option}" in
    d) DETACH=1;;
  esac
done

if [ -z $DETACH ]; then
  ./cloudstore/rel/cloudstore/bin/cloudstore console
else
  echo 'Starting CloudStore as daemon'
  ./cloudstore/rel/cloudstore/bin/cloudstore console &
fi
