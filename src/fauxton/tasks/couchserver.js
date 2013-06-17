// Licensed under the Apache License, Version 2.0 (the "License"); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

module.exports = function (grunt) {
  var log = grunt.log;

 grunt.registerTask("couchserver", 'Run a couch dev proxy server', function () {
    var fs = require("fs"),
    path = require("path"),
    httpProxy = require('http-proxy'),
    express = require("express"),
    options = grunt.config('couchserver'),
    app = express();

    // Options
    var dist_dir = options.dist || './dist/debug/';
    var port = options.port || 8000;

    // Proxy options with default localhost
    var proxy_settings = options.proxy || {
      target: {
        host: 'localhost',
        port: 5984,
        https: false
      }
    };

    // inform grunt that this task is async
    var done = this.async();

    // serve any javascript or css files from here
    app.get(/\.css$|\.js$|img/, function (req, res) {
      res.sendfile(path.join(dist_dir,req.url));
    });

    // create proxy to couch for all couch requests
    var proxy = new httpProxy.HttpProxy(proxy_settings);

    // serve main index file from here
    // Also proxy out to the base CouchDB host for handle_welcome_req.
    // We still need to reach the top level CouchDB host even through
    // the proxy.
    app.get('/', function (req, res) {
      var accept = req.headers.accept.split(',');
      if (accept[0] == 'application/json') {
        proxy.proxyRequest(req, res);
      } else {
        res.sendfile(path.join(dist_dir, 'index.html'));
      }
    });

    app.all('*', function (req, res) {
      proxy.proxyRequest(req, res);
    });

    // Fail this task if any errors have been logged
    if (grunt.errors) {
      return false;
    }

    var watch = grunt.util.spawn({cmd: 'bbb', grunt: true, args: ['watch']}, function (error, result, code) {/* log.writeln(String(result));*/ });

    watch.stdout.pipe(process.stdout);
    watch.stderr.pipe(process.stderr);

    log.writeln('Listening on ' + port);
    app.listen(port);

  });

};
