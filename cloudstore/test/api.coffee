assert  = require 'assert'
#chai    = require 'chai'
http    = require 'http'
uuid    = require 'node-uuid'
config  = require './config'

describe 'Cloud Store API', ->
  TEST_NAMESPACE = "/__tests__/#{uuid.v4()}"
  USERID = uuid.v4()
  TOKEN = uuid.v4()
  COOKIE = "token=#{TOKEN}"

  # cleanup test data
  after ->
    req = http.request(httpOptions('DELETE', "#{TEST_NAMESPACE}"))
    req.end()

  user =
    username    : 'Bilbo Baggins'
    email       : 'bilbo@hobbits.co.uk'
    firstName   : 'Bilbo'
    lastName    : 'Baggins'

  httpOptions = (method, path, cookie = null, json = null) ->
    ret = {
      host: config.DOMAIN
      port: config.PORT
      path: path
      method: method
      headers:
        'Content-Type': 'application/json'
    }
    ret['headers']['Content-Length'] = json.length if json
    ret['headers']['Cookie'] = cookie if cookie
    ret

  assertEmpty = ->
    it 'should retrieve an empty object', (done) ->
      opts = httpOptions('GET', "#{TEST_NAMESPACE}/users/#{USERID}/*", COOKIE)
      req = http.request(opts, (res) ->
        assert.equal 200, res.statusCode
        res.on 'data', (chunk) ->
          assert.equal Object.keys(JSON.parse(chunk)).length, 0
        done()
      )
      req.on 'error', (e) ->
        done()
      req.end()

  it 'should set auth token', (done) ->
    auth = {}
    auth["#{TEST_NAMESPACE}/users/#{USERID}"] = 'rw'
    json = JSON.stringify(auth)
    req = http.request(httpOptions('PUT', "/tokens/#{TOKEN}", null, json), (res) ->
      assert.equal 204, res.statusCode
      res.on 'data', (chunk) ->
        assert.equal Object.keys(JSON.parse(chunk)).length, 0
      done()
    )
    req.on 'error', (e) ->
      done()
    req.write json
    req.end()

  assertEmpty()

  it 'should set an object', (done) ->
    json = JSON.stringify(user)
    req = http.request(httpOptions('PUT', "#{TEST_NAMESPACE}/users/#{USERID}", COOKIE, json), (res) ->
      assert.equal 204, res.statusCode
      res.on 'data', (chunk) ->
        assert.equal Object.keys(JSON.parse(chunk)).length, 0
      done()
    )
    req.on 'error', (e) ->
      done()
    req.write json
    req.end()

  it 'should retrieve an object', (done) ->
    req = http.request(httpOptions('GET', "#{TEST_NAMESPACE}/users/#{USERID}/*", COOKIE), (res) ->
      assert.equal 200, res.statusCode
      res.on 'data', (chunk) ->
        data = JSON.parse(chunk)
        objectPath = "#{TEST_NAMESPACE}/users/#{USERID}"
        assert.equal Object.keys(data).length, 1
        assert.equal Object.keys(data[objectPath]).length, 4
        assert.equal data[objectPath]['username'], 'Bilbo Baggins'
        assert.equal data[objectPath]['email'], 'bilbo@hobbits.co.uk'
        assert.equal data[objectPath]['firstName'], 'Bilbo'
        assert.equal data[objectPath]['lastName'], 'Baggins'

      done()
    )
    req.on 'error', (e) ->
      done()
    req.end()

  it 'should delete an object', (done) ->
    req = http.request(httpOptions('DELETE', "#{TEST_NAMESPACE}/users/#{USERID}", COOKIE), (res) ->
      assert.equal 204, res.statusCode
      done()
    )
    req.on 'error', (e) ->
      done()
    req.end()

  assertEmpty()
