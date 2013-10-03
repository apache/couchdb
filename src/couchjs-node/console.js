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
var util = require('util');

var noop = function() {};

module.exports = {};
module.exports.log = noop;
module.exports.debug = noop;
module.exports.info = noop;
module.exports.warn = noop;
module.exports.error = noop;

var LOG_PATH = '/tmp/couchjs.log';
var stat = null;
var LOG = null;

try {
  stat = fs.statSync(LOG_PATH);
} catch(er) {}

if (stat) {
  LOG = fs.createWriteStream(LOG_PATH, {
    'flags':'a'
  });

  var log = function () {
    var str = util.format.apply(this, arguments);
    LOG.write(str + '\n');
  };

  var on_err = function (er) {
    module.exports.error('Uncaught error:\n%s', er.stack || er.message || JSON.stringify(er));

    if (er.stack) {
      er = ['fatal', 'unknown_error', er.stack];
    }

    process.stdout.write(JSON.stringify(er) + '\n');
    process.exit(1);
  };

  module.exports.log = log;
  module.exports.debug = log;
  module.exports.info = log;
  module.exports.warn = log;
  module.exports.error = log;

  process.on('exit', function() {
    module.exports.log('Exit %d', process.pid);
  });

  process.on('uncaughtException', on_err);
}

