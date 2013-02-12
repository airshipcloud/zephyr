config  = require './config'
express = require 'express'
http    = require 'http'
uuid = require 'node-uuid'
app = express()

app.get('/auth', (req, res) ->
  uid = uuid()
  token = uuid()
  setToken(uid, token)

  redirectUrl = req.query['redirect_url']
  res.redirect(redirectUrl + "?uid=#{uid}&token=#{token}")
)

app.get('/auth_test', (req, res) ->
  uid = uuid()
  token = uuid()
  setToken(uid, token)

  res.write("uid=#{uid}\ntoken=#{token}")
  res.end()
)

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

setToken = (uid, token) ->
  auth = {}
  auth["/users/#{uid}"] = 'rw'
  json = JSON.stringify(auth)
  req = http.request(httpOptions('PUT', "/tokens/#{token}", null, json), (res) ->
    res.on 'data', (chunk) ->
      assert.equal Object.keys(JSON.parse(chunk)).length, 0
  )
  req.on 'error', (e) ->
    throw new Error(e)
  req.write json
  req.end()

app.listen(3001)
console.log('Listening on port 3001')
