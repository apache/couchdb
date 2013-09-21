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

module.exports = { 'print'   : print
                 , 'readline': readline
                 , 'stdin'   : stdin
                 , 'evalcx'  : evalcx
                 , 'quit'    : quit
                 , 'gc'      : gc
                 }


var vm = require('vm')
var Fiber = require('fibers')

var XML = require('./xml')
var log = require('./console').log

var INPUT = {'queue':[], 'waiting':null}

Error.prototype.toSource = Error.prototype.toSource || toSource
Error.prototype.toString = Error.prototype.toString || toSource
Function.prototype.toSource = Function.prototype.toSource || toSource
Function.prototype.toString = Function.prototype.toString || toSource


function print(line) {
  log('STDOUT %s: %s', process.pid, line)
  process.stdout.write(line + '\n')

  try {
    line = JSON.parse(line)
  } catch(er) { return }

  if(line[0] == 'log')
    log('LOG: %s', line[1])
}

function stdin(line) {
  log('STDIN %s: %s', process.pid, line.trim())
  if(INPUT.waiting)
    INPUT.waiting.run(line)
  else
    INPUT.queue.push(line)
}

function readline() {
  var line = INPUT.queue.shift()
  if(line)
    return line

  INPUT.waiting = Fiber.current
  line = Fiber.yield()
  INPUT.waiting = null

  return line
}


function evalcx(source, sandbox) {
  sandbox = sandbox || {}
  //log('evalcx in %j: %j', Object.keys(sandbox), source)

  if(source == '')
    return sandbox

  // source might be "function(doc) { emit(doc._id, 1) }"
  source = source.replace(/;+$/, '')

  sandbox.XML = sandbox.XML || XML
  source = '(' + source + ')'

  try {
    var id = Math.floor(Math.random() * 1000*1000)
    var filename = '_couchdb:' + id + '.js'
    var script = vm.createScript(source, filename)
    var func = script.runInNewContext(sandbox)
  } catch (er) {
    log('Error making code: %s', er.stack)
    return sandbox
  }

  return func
}

function quit(code) {
  code = code || 1
  if(code < 0)
    code = -code

  process.exit(code)
}

function gc() { }


function toSource() {
  if(typeof this == 'function')
    return '' + this

  if(this instanceof Error)
    return this.stack

  return util.inspect(this)
}
