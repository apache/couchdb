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

module.exports = start

if(require.main === module)
  main()


var fs = require('fs')
var util = require('util')
var child_process = require('child_process')

var log = require('./console').log

function start(debugPort) {
  if(!debugPort || typeof debugPort != 'number')
    throw new Error('Need a listen debugPort')
  var webPort = debugPort + 1

  var cmd = __filename
  var args = [debugPort, webPort]
  var opts =
    { 'cwd': __dirname
    , 'stdio': 'pipe'
    , 'detached': false
    }

  log('Start inspector: %s %j %j', cmd, args, opts)
  var inspector = child_process.spawn(cmd, args, opts)
  watch_inspector(inspector)

  log('Enable remote debug pid=%d port=%d', process.pid, debugPort)
  process.debugPort = debugPort
  process.kill(process.pid, 'SIGUSR1')
}

function watch_inspector(child) {
  child.stderr.on('data', function(body) {
    log('Inspector STDERR: %s', body)
  })
  child.stdout.on('data', function(body) {
    log('Inspector STDOUT: %s', body)
  })

  child.on('exit', function(code, signal) {
    log('Inspector exited %d signal=%j', code, signal)
    process.exit(code)
  })

  process.on('exit', function() {
    log('Kill inspector upon exit: %d', child.pid)
    process.kill(child.pid, 'SIGTERM')
  })
}


function main() {
  var debugPort = +process.argv[2]
  var webPort = +process.argv[3]

  if(!debugPort || !webPort)
    throw new Error('Bad arguments: need debugPort and webPort')

  console.log('Start inspector debugPort=%j webPort=%j', debugPort, webPort)
  var DebugServer = require('node-inspector/lib/debug-server')
  var server = new DebugServer
  server.on('close', function() {
    console.log('Server closed')
    process.exit(0)
  })

  server.start({'webPort':webPort, 'debugPort':debugPort})
  process.on('uncaughtException', function(er) {
    console.log('Error:\n%s', er.stack)
  })
}
