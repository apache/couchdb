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

// Text line stream

module.exports = LineStream
module.exports.v2 = LineStream2

var stream = require('stream')
var util = require('util')


util.inherits(LineStream2, stream.Transform)
function LineStream2 () {
  if(! (this instanceof LineStream2))
    return new LineStream2

  stream.Transform.call(this)
  this.setEncoding('utf8')
}

LineStream2.prototype._transform = function(message, encoding, done) {
  var self = this

  message = message.toString(encoding)
  var lines = message.split(/\n/)

  // If the data ends in "\n" this will be ""; otherwise the final partial line.
  var remainder = lines.pop()
  if(remainder)
    this.unshift(remainder)

  lines.forEach(function(line) {
    self.push(line)
  })

  done()
}

util.inherits(LineStream, stream)
function LineStream () {
  var self = this
  stream.call(self)

  self.readable = true
  self.writable = true

  self.buffer = ''
  self.downstream = null

  self.on('pipe', function(upstream) {
    upstream.on('end', function(data, encoding) {
      self.emit('end', data, encoding)
    })
  })
}


LineStream.prototype.write = function(data, encoding) {
  var self = this

  data = data || ''
  if(typeof data != 'string')
    return self.error(new Error('Data was not a string: ' + util.inspect(data)))

  self.buffer += data
  var lines = self.buffer.split(/\n/)
  self.buffer = lines.pop() // If the data ended in "\n" this will be ""; otherwise the final partial line.

  lines.forEach(function(line) {
    self.emit('data', line)
  })
}


LineStream.prototype.end = function(data, encoding) {
  var self = this

  self.is_ending = true
  self.writable = false

  // Always call write, even with no data, so it can fire the "end" event.
  self.write(data)
}


LineStream.prototype.error = function(er) {
  var self = this

  self.readable = false
  self.writable = false
  self.emit('error', er)

  // The write() method sometimes returns this value, so if there was an error, make write() return false.
  return false
}
