var assert = require("assert")
var http = require("http")
var uuid = require("node-uuid")

require("./config")

describe('Basic', function() {
    var userId = uuid.v4();
    var user = {
        username: 'Bilbo Baggins',
        email: 'bilbo@hobbits.co.uk',
        firstName: 'Bilbo',
        lastName: 'Baggins'
    };
    var retrieveEmpty = function () {
        it('should retrieve an empty object', function(done) {
            var options = {
                host: CLOUDSTORE_DOMAIN,
                port: CLOUDSTORE_PORT,
                path: '/users/' + userId + '/*',
                method: 'GET',
                headers: {
                    'Content-Type': 'application/json'
                }
            };
            var req = http.request(options, function(res) {
                assert.equal(200, res.statusCode);
                res.on('data', function(chunk) {
                    assert.equal(Object.keys(JSON.parse(chunk)).length, 0);
                });
                done();
            })
            req.on('error', function(e) {
                done();
            });
            req.end();
        });
    };
    retrieveEmpty();
    it('should set an object', function(done) {
        var json = JSON.stringify(user);
        var options = {
            host: CLOUDSTORE_DOMAIN,
            port: CLOUDSTORE_PORT,
            path: '/users/' + userId,
            method: 'PUT',
            headers: {
                'Content-Type': 'application/json',
                'Content-Length': json.length
            }
        };
        var req = http.request(options, function(res) {
            assert.equal(204, res.statusCode);
            res.on('data', function(chunk) {
                assert.equal(Object.keys(JSON.parse(chunk)).length, 0);
            });
            done();
        })
        req.on('error', function(e) {
            done();
        });
        req.write(json);
        req.end();
    });
    it('should retrieve an object', function(done) {
        var options = {
            host: CLOUDSTORE_DOMAIN,
            port: CLOUDSTORE_PORT,
            path: '/users/' + userId + '/*',
            method: 'GET',
            headers: {
                'Content-Type': 'application/json'
            }
        };
        var req = http.request(options, function(res) {
            assert.equal(200, res.statusCode);
            res.on('data', function(chunk) {
                var data = JSON.parse(chunk);
                var objectPath = '/users/' + userId;
                assert.equal(Object.keys(data).length, 1);
                assert.equal(Object.keys(data[objectPath]).length, 4);
                assert.equal(data[objectPath]['username'], 'Bilbo Baggins');
                assert.equal(data[objectPath]['email'], 'bilbo@hobbits.co.uk');
                assert.equal(data[objectPath]['firstName'], 'Bilbo');
                assert.equal(data[objectPath]['lastName'], 'Baggins');
            });
            done();
        })
        req.on('error', function(e) {
            done();
        });
        req.end();
    });
    it('should delete an object', function(done) {
        var options = {
            host: CLOUDSTORE_DOMAIN,
            port: CLOUDSTORE_PORT,
            path: '/users/' + userId,
            method: 'DELETE',
            headers: {
                'Content-Type': 'application/json'
            }
        };
        var req = http.request(options, function(res) {
            assert.equal(204, res.statusCode);
            done();
        })
        req.on('error', function(e) {
            done();
        });
        req.end();
    });
    retrieveEmpty();
})
