#!/usr/bin/env node

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

var fs = require('fs');
var Fiber = require('fibers');
var optimist = require('optimist');

var couchjs = require('./couchjs');
var package_json = require('./package.json');
var LineStream = require('./stream');
var inspector = require('./inspector');
var log = require('./console').log;


var opts = optimist.boolean(['h', 'V', 'H'])
                   .describe({ 'h': 'display a short help message and exit',
                             'V': 'display version information and exit',
                             'H': 'enable couchjs cURL bindings (not implemented)'
                             })
                   .usage('$0 <path to main.js>');


function main() {

  'use strict';

  var main_js = opts.argv._[0];

  if (!main_js) {
    return console.error(opts.help());
  }

  log('couchjs/%s %s: %s', package_json.version, process.pid, main_js);

  if (process.env.COUCHJS_DEBUG_PORT) {
    inspector(+process.env.COUCHJS_DEBUG_PORT);
  }

  fs.readFile(main_js, 'utf8', function(er, body) {
    if (er) {
      throw er;
    }

    var stdin = new LineStream.v2();

    stdin.on('readable', function() {
      var buf = stdin.read();

      if (buf) {
        couchjs.stdin(buf);
      }
    });

    stdin.on('end', function() {
      log('Terminate; connection to parent closed');
      process.exit(0);
    });

    process.stdin.setEncoding('utf8');
    process.stdin.pipe(stdin);

    var main_func = Function(['print', 'readline', 'evalcx', 'gc', 'quit'], body);

    log('Call main');

    new Fiber(function() {
      main_func(couchjs.print, couchjs.readline, couchjs.evalcx, couchjs.gc);
    }).run();
  });

  process.on('uncaughtException', function(er) {
    log('Error:\n%s', er.stack);
  });
}

if (require.main === module) {
  main();
}
