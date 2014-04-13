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
define([
       'addons/logs/base',
       'chai'
], function (Log, chai) {
  var expect = chai.expect;

  describe('Logs Addon', function(){

    describe('Log Model', function () {
      var log;

      beforeEach(function () {
        log = new Log.Model({
          log_level: 'DEBUG',
          pid: '1234',
          args: 'testing 123',
          date: (new Date()).toString()
        });
      });

      it('should have a log level', function () {
        expect(log.logLevel()).to.equal('DEBUG');
      });

    });
  });
});
