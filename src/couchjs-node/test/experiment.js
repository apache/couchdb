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

var vm = require('vm');
var util = require('util');

var STATE = 'wait';

function main() {
  process.debugPort = 5859;
  process.kill(process.pid, 'SIGUSR1');

  setTimeout(function() {
    stuff(0);
  }, 1000);
}

function stuff(count) {

  console.log('Doing stuff: %d', count);
  STATE = 'vm';
  console.log('More stuff: %d', count);

  if (STATE === 'done') {
    console.log('Done');
  } else if (STATE === 'code') {
    setTimeout(code, 1000);
  } else if(STATE === 'eval') {
    test_eval();
  } else if(STATE === 'vm') {
    test_vm();
  } else if(STATE === 'wait') {
    setTimeout(function() {
      stuff(count+1);
    }, 1000);
  } else {
    throw new Error('Unknown state: ' + STATE);
  }
}

function code() {
  var code = [
    'var foo = "in the code"',
    'console.log("This is some code")',
    'debugger',
    'console.log("foo = " + foo)'
    ].join('\n');

  var runner = Function([], code);

  console.log('Run runner in 1s');

  setTimeout(run_runner, 1000)

  function run_runner() {
    console.log('About to run runner');
    debugger;
    runner();
    console.log('Runner done');
  }
}

function test_eval() {
  console.log('Test eval in 1s');
  setTimeout(run_eval, 1000);

  var code = [
    'var foo = "in eval"',
    'console.log("This is eval")',
    'debugger',
    'console.log("foo = " + foo)'
    ].join('\n');

  function run_eval() {
    console.log('Run eval now');
    debugger;
    eval(code);
  }
}

function test_vm() {
  console.log('Test vm');

  var code = [
    'var i = 10',
    'setTimeout(hello, 1000)',
    '',
    'function hello() {',
    '  debugger',
    '  console.log("Hello: " + i)',
    '  if(--i)',
    '    setTimeout(hello, 1000)',
    '}'
  ].join('\n');

  console.log('Run vm now');
  var filename = '_couchdb:code.js';

  var sandbox = {};
  var ok = ['console', 'setTimeout'];

  ok.forEach(function(key) {
    sandbox[key] = global[key];
  });

  var ctx = vm.createContext(sandbox);
  var script = vm.createScript(code, filename);

  var r = script.runInNewContext(sandbox);
  console.log('Result:\n%s', util.inspect(r, false, 10));
  return r;
}

if (require.main === module) {
  main();
}
