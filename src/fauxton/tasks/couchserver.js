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
        http = require("http"),
        httpProxy = require('http-proxy'),
        send = require('send'),
        urlLib = require('url'),
        options = grunt.config('couchserver'),
        _ = grunt.util._;

    // Options
    var dist_dir = options.dist || './dist/debug/',
        app_dir = './app',
        port = options.port || 8000,
        setContentSecurityPolicy = _.isUndefined(options.contentSecurityPolicy) ? true : options.contentSecurityPolicy;

    // Proxy options with default localhost
    var proxy_settings = options.proxy || {
      target: "http://localhost:5984/"
    };

    // inform grunt that this task is async
    var done = this.async();

    // create proxy to couch for all couch requests
    var proxy = httpProxy.createServer(proxy_settings);

    http.createServer(function (req, res) {
      var url = req.url.replace('app/',''),
          accept = req.headers.accept.split(','),
          filePath;

      if (setContentSecurityPolicy) {
        var headerValue = "default-src 'self'; img-src 'self'; font-src 'self'; " +
                          "script-src 'self' 'unsafe-eval'; style-src 'self' 'unsafe-inline';";
        res.setHeader('Content-Security-Policy', headerValue);
      }

      if (!!url.match(/^\/addons\/.*\/assets\/js/)) {
        filePath = path.join(app_dir, url.replace('/_utils/fauxton/',''));
      } else if (!!url.match(/assets/)) {
        // serve any javascript or css files from here assets dir
        url = url.replace(/\?.*/, '');
        filePath = path.join('./',url);
      } else if (!!url.match(/mocha|\/test\/core\/|test\.config/)) {
        filePath = path.join('./test', url.replace('/test/',''));
      } else if (!!url.match(/\.css|img/)) {
        url = url.replace(/\?.*/, '');
        filePath = path.join(dist_dir,url);
      /*} else if (!!url.match(/\/js\//)) {
        // serve any javascript or files from dist debug dir
        filePath = path.join(dist_dir,req.url);*/
      } else if (!!url.match(/\.js$|\.html$/)) {
        // server js from app directory
        filePath = path.join(app_dir, url.replace('/_utils/fauxton/',''));
      } else if (!!url.match(/testrunner/)) {
        var testSetup = grunt.util.spawn({cmd: 'grunt', grunt: true, args: ['test_inline']}, function (error, result, code) {/* log.writeln(String(result));*/ });
        testSetup.stdout.pipe(process.stdout);
        testSetup.stderr.pipe(process.stderr);
        filePath = path.join('./test/runner.html');
      } else if (url === '/' && accept[0] !== 'application/json') {
        // serve main index file from here
        filePath = path.join(dist_dir, 'index.html');
      };

      if (filePath) {
        return send(req, filePath)
          .on('error', function (err) {
            if (err.status === 404) {
              log.writeln('Could not locate', filePath);
            } else {
              log.writeln('ERROR', filePath, err);
            }

            res.setHeader("Content-Type", "text/javascript");
            res.statusCode = 404;
            res.end(JSON.stringify({error: err.message}));
          })
          .pipe(res);
      }

      // This sets the Host header in the proxy so that one can use external
      // CouchDB instances and not have the Host set to 'localhost'
      var urlObj = urlLib.parse(req.url);
      req.headers['host'] = urlObj.host;

      proxy.web(req, res);
    }).listen(port);

    // Fail this task if any errors have been logged
    if (grunt.errors) {
      return false;
    }

    var watch = grunt.util.spawn({cmd: 'grunt', grunt: true, args: ['watch']}, function (error, result, code) {/* log.writeln(String(result));*/ });

    watch.stdout.pipe(process.stdout);
    watch.stderr.pipe(process.stderr);

    var logo = [
      [""],
      [" ______                        _                   "],
      ["|  ____|                      | |                  "],
      ["| |__    __ _   _   _  __  __ | |_    ___    _ __  "],
      ["|  __|  / _` | | | | | \\ \\/ / | __|  / _ \\  | '_ \\ "],
      ["| |    | (_| | | |_| |  >  <  | |_  | (_) | | | | |"],
      ["|_|     \\__,_|  \\__,_| /_/\\_\\  \\__|  \\___/  |_| |_|"],
      [""]
   ];

    _.each(logo, function (line) {
      console.log(line.toString());
    });

    log.writeln('Listening on ' + port);
  });

};
