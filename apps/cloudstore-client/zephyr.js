var ZephyrBase = function (uid, token, path) {
    var self = this;
    self.uid = uid;
    self.token = token;
    self.path = path;
    self.etag = null;
    self._put = function (data, mutator) {
        $.ajax({
            type: 'PUT',
            dataType: 'json',
            url: 'http://127.0.0.1:10002/' + self.uid + '/' + self.path + '?token=' + self.token,
            contentType: 'application/json',
            data: JSON.stringify(data),
            //xhrFields: { withCredentials: true }, -- we don't need this until we need to pass cookies
            beforeSend: function (req) {
                //req.setRequestHeader("if-match", self.etag);
            },
            success: function () {
                // refresh etag
                self._get();
            },
            error: function () {
                if (mutator) {
                    // concurrent put detected -- retry
                    self._get(mutator);
                }
            }
        });
    };
    self._get = function (mutator) {
        $.ajax({
            type: 'GET',
            dataType: 'json',
            url: 'http://127.0.0.1:10002/' + self.uid + '/' + self.path + '?token=' + self.token,
            success: function (data, status, xhr) {
                var etag = xhr.getResponseHeader('etag');
                if (etag !== self.etag) {
                    // data changed since last seen
                    self.etag = etag;
                    if (!mutator) {
                        self._fireChange(data);
                    }
                }
                if (mutator) {
                    self._put(mutator(data), mutator);
                }
            },
            error: function () {
                if (mutator) {
                    // not found -- mutate undefined
                    self._put(mutator(undefined));
                }
            }
        });
    };
    self._fireChange = function (data) {
        var onchange = self.onchange;
        if (onchange) {
            onchange(data);
        }
    };
    self.putSafe = function (mutator) {
        self._get(mutator);
    };
    self.put = function (data) {
        self._put(data);
    };
    self._get();
    self.poll = window.setInterval(function() { self._get(); }, 1000);
};
