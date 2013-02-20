[![Build Status](https://api.travis-ci.org/airships/zephyr.png)](https://travis-ci.org/airships/zephyr)

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



## PROJECT STATUS

**Zephyr is a new project** but moving quickly to solve real world app developer problems.

* **Router** – config and basic router [done]
* **CloudStore** – token based authentication, GET, PUT, DELETE [done]
* **Tests** – basic coverage for CloudStore [done]
* **Example Apps** – Chat [done], data browser [WIP]
* **3rd Party API Support**
  * [Singly](http://singly.com) for fetching social and personal data [WIP]
  * [remoteStorage](http://remotestorage.io) for unhosted apps [WIP]
  * connectors for Dropbox, Facebook, Twitter, Gmail, etc. [not started]



## ARCHITECTURE OVERVIEW

**Zephyr is composed of several modular components** that interact via HTTP APIs.

**Components:**

1. **[Router](https://github.com/airships/zephyr/wiki/Routes):** API endpoint that routes requests to other components
2. **[CloudStore](https://github.com/airships/zephyr/wiki/CloudStore-API):**
   key/value data store with OAuth security layer and notification events
3. **[Pub/Sub](https://github.com/airships/zephyr/wiki/PubSub-API):** webhook callback engine for distributed event notification
4. **Engines:** pluggable components to fetch social data, provide additional services, etc.

Each component is defined by a RESTful API and is completely replaceable by anything that provides the same API.
Most components run behind a firewall and talk to each other on the internal network.
Components can be spread out across any number of servers or run on a single server.



## REQUIREMENTS

* [Erlang R15B03+](https://www.erlang-solutions.com/downloads/download-erlang-otp) – high-performance core components
* [Postgres 9.1+](http://www.postgresql.org/) with [hstore extension](http://www.postgresql.org/docs/9.1/static/hstore.html) – data store
* [Node.js 0.8+](http://nodejs.org/) – apps, tests, client libraries, etc.

Zephyr runs on OSX and Linux. Windows has not yet been tested.



## INSTALLATION

**OSX Quick Install**

```bash
brew install erlang
brew install postgres
brew install nodejs

# see Install Guide for postgres pg_hba.conf issues
```

[Install Guide](https://github.com/airships/zephyr/wiki/Install) for Linux and detailed instructions



## QUICK START

```bash
# make sure postgres is already running

git clone https://github.com/airships/zephyr.git
cd zephyr
make setup
script/zephyr start
make test
```



## CONFIGURATION

Configuration options are in [config directory](https://github.com/airships/zephyr/tree/master/config) files.
When an option is changed, recompile and restart services.

```bash
# reapply config and recompile
make
# restart services
script/zephyr restart
```

[Config Guide](https://github.com/airships/zephyr/wiki/Configuration) for options and details



## RUNNING

```bash

# start console in foreground
script/zephyr console

# start as service
script/zephyr start

# stop service
script/zephyr stop

# restart service
script/zephyr restart
```



## DATA API OVERVIEW

When Zephyr starts, it runs two data services:

1. Data API
2. Token Service

The Data API provides RESTful access to data. The Token Service allows for the creation and deletion of access tokens.
For security reasons, the Token Service listens on a different port and (optionally) network interface.


**Add Auth Token**

An auth token is required to access data.

```bash
curl --request PUT --header "Content-Type: application/json" --data "{\"\":\"rw\"}" --verbose http://127.0.0.1:10004/tokens/SECRET
```

In browser or curl, GET [http://127.0.0.1:10002/*?token=SECRET](http://127.0.0.1:10002/*?token=SECRET) which should return an empty JSON object.

The token can be passed in as a cookie or query param.


**PUT**

```bash
curl --data "{\"foo\":\"bar\"}" --request PUT --header "Content-Type: application/json" --verbose http://127.0.0.1:10002/foo/bar/baz?token=SECRET
```

**GET**

```bash
curl --verbose http://127.0.0.1:10002/foo/bar/baz?token=SECRET
```

**DELETE**

```bash
curl --request DELETE --verbose http://127.0.0.1:10002/foo/bar/baz?token=SECRET
```

Note that PUT is a merge operation for objects.
This makes it easy for clients to update specific attributes of an existing object without needing to send the entire object.

```bash
curl --data "{\"foo\":\"bar\"}" --request PUT --header "Content-Type: application/json" --verbose http://127.0.0.1:10002/foo/bar/baz?token=SECRET
curl --data "{\"new\":\"bar\"}" --request PUT --header "Content-Type: application/json" --verbose http://127.0.0.1:10002/foo/bar/baz?token=SECRET
curl --verbose http://127.0.0.1:10002/foo/bar/baz?token=SECRET

# Returns:
# {
#   "foo": "bar",
#   "new": "bar"
# }
```

To replace the entire object, use REPLACE or do a DELETE then PUT.

[Data API Documenation](https://github.com/airships/zephyr/wiki/CloudStore-API)



## TESTING

API tests are written in Node.js [mocha](http://visionmedia.github.com/mocha/)

```bash
# run tests
make test
```

Zephyr must be already running for the tests to pass. Tests are run against the same database
as development but on a guaranteed unique test branch that is deleted at the end of the tests.

[Testing Guide](https://github.com/airships/zephyr/wiki/Testing).



## DOCUMENTATION

[API and Reference](https://github.com/airships/zephyr/wiki) documentation are in the wiki.



## TROUBLESHOOTING & COMMUNITY SUPPORT

* [**Google+ Community**](https://plus.google.com/u/1/communities/107361427153729973121) – get quick answers from the community
* [**Troubleshooting**](https://github.com/airships/zephyr/wiki/Troubleshooting) – wiki page for common issues

Erlang compilation errors (usually rebar errors) are typically caused by having the wrong version of Erlang/OTP installed.
Refer to [**Troubleshooting**](https://github.com/airships/zephyr/wiki/Troubleshooting) for help.



## CONTRIBUTING

Join the [Google+ Community](https://plus.google.com/u/1/communities/107361427153729973121) or submit a pull request.

We especially appreciate hearing about your app ideas and how we can help make your life easier as a developer.


