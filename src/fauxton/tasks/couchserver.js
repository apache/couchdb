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
    var proxy_target = options.proxy || {
          host: 'localhost',
          port: 5984,
          https: false
        };

    // inform grunt that this task is async
    var done = this.async();

    // images don't have the full path, so have to have own route for them
    app.get('/img/*', function (req, res) {
      res.sendfile(path.join(dist_dir,req.url));
    });

    // serve any javascript or css files from here
    app.get('/assets/*', function (req, res) {
      res.sendfile(req.url.replace('/assets/',dist_dir));
    });

    // serve main index file from here
    app.get('/', function (req, res) {
      res.sendfile(path.join(dist_dir, 'index.html'));
    });

    // create proxy to couch for all couch requests
    var proxy = new httpProxy.HttpProxy({ 
      target: proxy_target
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
