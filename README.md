Cloud Fabric is an open source stack to make scalable, real-time apps on top of a network of personal data stores.

# Overview

Cloud Fabric is a collection of modular components that provide the following services:

1. Scalable data store with a fully customizable security layer
2. Real-time event notification when data changes
3. Automatic networking and syncing with other Cloud Fabric servers


# Philosophy

Cloud Fabric can run as a lightweight personal cloud server to manage a single user's personal data or as a scalable app server framework.

With Cloud Fabric, you can easily create apps in HTML and JavaScript without the need to write server code. However, as your app complexity grows, you can add server-side logic in any language.

As a philosophy, Cloud Fabric is less of a traditional framework and more of a set of interoperable service APIs. The services talk to each other on an internal network and expose one external endpoint for communication with apps and other Cloud Fabric servers. Any service can be exchanged or customized without affect to the other services as long as the APIs remain the same.

The code provided in the repository is meant to eventually serve as a fully functional, production ready server. However, any server or component that conforms to the Cloud Fabric API will be linked to from this document.

# Installation

    git clone https://github.com/respectio/cloudfabric.git


**Requirements**

* [Erlang R15B03](https://www.erlang-solutions.com/downloads/download-erlang-otp)
* [Postgres 9.1+](http://www.postgresql.org/)

**OSX**

    brew install erlang
    brew install postgres

**Debian**

For Erlang, follow instructions at https://www.erlang-solutions.com/downloads/download-erlang-otp

For PostgreSQL 9.1+ on Debian Squeeze, install from [backports](http://backports-master.debian.org/Instructions/).

    apt-get -t squeeze-backports install postgresql-9.1


# Configuration

Edit config/base to change database settings, HTTP port, etc. then apply config and compile.

1. $ ./apply_config.sh
2. $ make


# Services

The Cloud Fabric stack is composed of several modular services that talk to each other over HTTP APIs.

## Cloud Store

**Create Postgres Database**

    cd cloudstore/db
    ./init.sh

Cloud Store provides a RESTful API for storing and retrieving data.

**Start Cloud Store**

    ./cloudstore/rel/cloudstore/bin/cloudstore console

[Cloud Store API Documenation](https://github.com/respectio/cloudfabric/wiki/CloudStore-API)

