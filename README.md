**zeph·yr** _noun_
* ~~a breeze from the west~~
* **a lightweight fabric**

**Zephyr is a lightweight personal cloud framework** that makes it easy to build
scalable, real-time apps powered by personal data.

Personal cloud is a relatively new term. In concept, it's a server that
provides authentication and access to a user's personal data. Think of
[Facebook](https://www.facebook.com) and [Dropbox](https://www.dropbox.com/)
as proto-personal clouds. Each one provides some form of authentication
and an API for fetching and storing data on behalf of the user. The problem
with existing proto-personal clouds is that they are neither standardized nor
portable which means it's not ideal for users and more work for app developers
to integrate.

[Learn more](https://github.com/airships/zephyr/wiki/Personal-Clouds)
about Personal Clouds.

**The goal of Zephyr** is to create a simple personal cloud framework that
provides standardized APIs for authenticating, fetching, normalizing,
storing, and sharing data between personal clouds and within a scalable
app architecture.

Support is being added to Zephyr for both existing personal data sources
(Facebook, Dropbox, Gmail, etc.) as well as fully compliant personal clouds.

Zephyr is designed to be used either as **your app's backend** or as a
**personal data cache layer** for your existing apps.



## Status

**Zephyr is a new project** but moving quickly to solve real world app developer problems.

* **CloudStore** – supports token based authentication, GET, PUT, DELETE
* **Tests** – basic coverage for CloudStore
* **Example Apps** – WIP
* **3rd Party API Support** – WIP
  * [Singly](http://singly.com) for fetching social and personal data
  * [remoteStorage](http://remotestorage.io) for unhosted apps
  * social connectors for Facebook, Twitter, LinkedIn, etc.
  * calendar connectors for Gmail, IMAP, etc.



## Components

**Zephyr is composed of several modular components** that interact via HTTP APIs.

**Components:**

1. **[Router](https://github.com/airships/zephyr/wiki/Routes):** API endpoint that routes requests to other components
2. **[CloudStore](https://github.com/airships/zephyr/wiki/CloudStore-API):**
   key/value data store with OAuth security layer and notification events
3. **Pub/Sub:** webhook callback engine for distributed event notification
4. **Engines:** pluggable components to fetch social data, provide additional services, etc.

Each component is defined by a RESTful API and is completely replaceable by anything that provides the same API.
Most components run behind a firewall and talk to each other on the internal network.
Components can be spread out across any number of servers or run on a single server.



## Installation

    git clone https://github.com/airships/zephyr.git


**Requirements**

* [Erlang R15B03](https://www.erlang-solutions.com/downloads/download-erlang-otp) – for core framework components
* [Postgres 9.1+](http://www.postgresql.org/) with [hstore extension](http://www.postgresql.org/docs/9.1/static/hstore.html) – for CloudStore
* [Node.JS](http://nodejs.org/) – (optional) for tests and compiling JavaScript client libaries

**OSX**

    brew install erlang
    brew install postgres
    brew install nodejs

**Debian**

For Erlang, follow instructions at https://www.erlang-solutions.com/downloads/download-erlang-otp

For PostgreSQL 9.1+ on Debian Squeeze, install from [backports](http://backports-master.debian.org/Instructions/).

    apt-get -t squeeze-backports install postgresql-9.1
    apt-get install postgresql-contrib-9.1
    apt-get nodejs
    apt-get npm



## Configuration

Edit config/base to change database settings, HTTP port, etc. then apply config and compile.

    ./apply_config.sh
    make

[Learn more](https://github.com/airships/zephyr/wiki/Configuration) in the wiki.

**Create Postgres Database**

    $ cd cloudstore/db
    $ ./init.sh

CloudStore provides a RESTful API for storing and retrieving data.



## Running

**Start CloudStore**

    $ ./cloudstore/rel/cloudstore/bin/cloudstore console

**Add Auth Token**

    $ curl --request PUT --header "Content-Type: application/json" --data "{\"\":\"rw\"}" --verbose http://127.0.0.1:10002/tokens/SECRET


In browser or curl, GET [http://127.0.0.1:10002/*?token=SECRET](http://127.0.0.1:10002/*?token=SECRET) which should return an empty JSON object.

The token can be passed in as a cookie or query param.



## Interacting with CloudStore

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



## Testing

API tests are written in [mocha](http://visionmedia.github.com/mocha/)

**Run CloudStore Tests**

    cd cloudstore
    npm install
    npm test

[Learn more](https://github.com/airships/zephyr/wiki/Testing).



## Troubleshooting & Support

If you run into a problem, just drop a note in the [Google+ Community](https://plus.google.com/u/1/communities/107361427153729973121).

Also refer to the [Troubleshooting](https://github.com/airships/zephyr/wiki/Troubleshooting) wiki page.



## Documentation

[API and reference](https://github.com/airships/zephyr/wiki) documentation are in the wiki.



## Contributing

Join the [Google+ Community](https://plus.google.com/u/1/communities/107361427153729973121) or submit a pull request.

We especially appreciate hearing about your app ideas and how we can help make your life easier as a developer.


