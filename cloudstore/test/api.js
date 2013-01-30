var assert = require("assert")
var http = require("http")
require("./config")

describe('Basic', function() {
    describe('GET', function() {
        it('should retrieve an empty object', function(done) {
            var options = {
                host: CLOUDSTORE_DOMAIN,
                port: CLOUDSTORE_PORT,
                path: '/users/u1/*',
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
        })
    });
})
