### Zephyr: OAuth + Firebase + Pub/Sub + Discovery

**zeph·yr** _noun_
* ~~a breeze from the west~~
* **a lightweight fabric**


Zephyr is a lightweight personal cloud framework that makes it easy to build
scalable, real-time apps powered by personal data.

A personal cloud is a relatively new term. In concept, it's a server that
provides authentication and access to a user's personal data. Think of Facebook
and Dropbox as personal clouds. Each one provides some form of authentication
and an API for fetching and storing data on behalf of the user. The problem
with existing proto-personal clouds is that they are neither standardized nor
portable which means it's not ideal for users and more work for app developers
to integrate. [Learn more](https://github.com/airships/zephyr/wiki/Personal-Clouds)
about Personal Clouds.

The goal of Zephyr is to create a simple personal cloud framework that provides
a set of standardized APIs for authenticating, fetching, normalizing, storing,
and sharing personal data between personal clouds and within a scalable app
architecture.



# Status

**Zephyr is a new project in its infancy but moving quickly.**

We're building Zephyr so that we can more quickly build other apps off of a common framework.
We also needed a reference implementation for a PDS endpoint to make PDSs interoperable.

The [CloudStore](https://github.com/airships/zephyr/wiki/CloudStore-API) is functional.
All other components are in the early stages of specing and documenting but are small in scope and should be available in Feb, 2013.

* [CloudStore](https://github.com/airships/zephyr/wiki/CloudStore-API): GET/PUT/DELETE supported, REPLACE is WIP
* Sharding: WIP
* Tests: basic CloudStore mocha tests complete
* Router: WIP
* Engines: XDI Engine in progress

# Overview

Zephyr is a collection of modular components that interact via HTTP APIs.

**Components:**

1. Router: API endpoint that routes requests to engines
2. CloudStore: data store with OAuth security layer and real-time event notifications
3. Engines + XDI: automatic networking and syncing with other personal data stores

![Zephyr Architecture](https://raw.github.com/respectio/cloudfabric/master/doc/images/cloudfabric_arch.png)



# Philosophy

Zephyr can run as a lightweight personal cloud server to manage a single user's personal data or as a scalable app server framework.

With Zephyr, you can easily create apps in HTML and JavaScript without the need to write server code. However, as your app complexity grows, you can add server-side logic in any language.

As a philosophy, Zephyr is less of a traditional framework and more of a set of interoperable service APIs. The services talk to each other on an internal network and expose one external endpoint for communication with apps and other Zephyr servers. Any service can be exchanged or customized without affect to the other services as long as the APIs remain the same.

The code provided in the repository is meant to eventually serve as a fully functional, production ready server. However, any server or component that conforms to the Zephyr API will be linked to from this document.

[Read background](#background) section for more details.




# Installation

    git clone https://github.com/airships/zephyr.git


**Requirements**

* [Erlang R15B03](https://www.erlang-solutions.com/downloads/download-erlang-otp) – for core framework components
* [Postgres 9.1+](http://www.postgresql.org/) with [hstore extension](http://www.postgresql.org/docs/9.1/static/hstore.html) – for CloudStore
* [Node.JS](http://nodejs.org/) – (optional) for tests and compiling JavaScript client libaries

**OSX**

    brew install erlang
    brew install postgres
    brew install nodejs
    npm install -g mocha

**Debian**

For Erlang, follow instructions at https://www.erlang-solutions.com/downloads/download-erlang-otp

For PostgreSQL 9.1+ on Debian Squeeze, install from [backports](http://backports-master.debian.org/Instructions/).

    apt-get -t squeeze-backports install postgresql-9.1
    apt-get install postgresql-contrib-9.1
    apt-get nodejs
    apt-get npm
    npm install -g mocha




# Configuration

Edit config/base to change database settings, HTTP port, etc. then apply config and compile.

    ./apply_config.sh
    make

[Learn more](https://github.com/airships/zephyr/wiki/Configuration) in the wiki.


# Services

The Zephyr stack is composed of several modular services that talk to each other over HTTP APIs.

## CloudStore

**Create Postgres Database**

    $ cd cloudstore/db
    $ ./init.sh

CloudStore provides a RESTful API for storing and retrieving data.

**Start CloudStore**

    $ ./cloudstore/rel/cloudstore/bin/cloudstore console

In browser or curl, GET [http://127.0.0.1:10002/*](http://127.0.0.1:10002/*) which should return an empty JSON object.


**Add Auth Token**

    $ curl --request PUT --header "Content-Type: application/json" --data "{\"\":\"rw\"}" --verbose http://127.0.0.1:10002/tokens/SECRET

Interact with CloudStore...

**PUT**

    curl --data "{\"foo\":\"bar\"}" --request PUT --header "Content-Type: application/json, Cookie: token=SECRET" --verbose http://127.0.0.1:10002/foo/bar/baz

**GET**

    curl --header "Cookie: token=SECRET" --verbose http://127.0.0.1:10002/foo/bar/baz

**DELETE**

    curl --header "Cookie: token=SECRET" --request DELETE --verbose http://127.0.0.1:10002/foo/bar/baz

Note that PUT is a merge operation for objects.
This makes it easy for clients to update specific attributes of an existing object without needing to send the entire object.

    curl --data "{\"foo\":\"bar\"}" --request PUT --header "Content-Type: application/json, Cookie: token=SECRET" --verbose http://127.0.0.1:10002/foo/bar/baz
    curl --data "{\"new\":\"bar\"}" --request PUT --header "Content-Type: application/json, Cookie: token=SECRET" --verbose http://127.0.0.1:10002/foo/bar/baz
    curl --header "Cookie: token=SECRET" --verbose http://127.0.0.1:10002/foo/bar/baz

    Returns:
    {
      "foo": "bar",
      "new": "bar"
    }

To replace the entire object, use REPLACE or do a DELETE then PUT.

[CloudStore API Documenation](https://github.com/airships/zephyr/wiki/CloudStore-API)


# Tests

API tests are written in [mocha](http://visionmedia.github.com/mocha/)

**Run CloudStore Tests**

    cd cloudstore
    npm install
    npm test

[Learn more](https://github.com/airships/zephyr/wiki/Testing) in the wiki.


# Background
<a id="background"></a>

Traditional apps are built around centralized servers with connectors to fetch data from other services.
If a user wishes to create a backup of app data, he/she must manually export the data to a personal data store (PDS) or harddrive.
Additionally, the app developer must manually support each 3rd party data provider.
![Traditional App Architecture](https://raw.github.com/respectio/cloudfabric/master/doc/images/app_arch_traditional.png)


Zephyr's Personal Data Store architecture puts users back in control of their data while making it easier for developers to create data rich and scalable apps.
In an ideal PDS architecture, user data is proxied through the user's PDS.
This enables auto-caching of data, a signle API endpoint for personal data, and a distributed architecture similar to email.
![Zephyr App Architecture](https://raw.github.com/respectio/cloudfabric/master/doc/images/app_arch_cloudfabric.png)

While Zephyr's ideal architecture uses the PDS as the user's single API data endpoint, almost any hybrid configuration is possible.
The main purpose of Zephyr is to make it easy to permission and manage distributed personal data.
As PDSs become more interoperable, more and more apps' architecture will look like the ideal diagram.


