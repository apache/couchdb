// Licensed under the Apache License, Version 2.0 (the 'License'); you may not
// use this file except in compliance with the License. You may obtain a copy of
// the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an 'AS IS' BASIS, WITHOUT
// WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
// License for the specific language governing permissions and limitations under
// the License.

define([
       'addons/logs/resources',
       'testUtils'
], function (Log, testUtils) {
  var assert = testUtils.assert;

  describe('Log Resources', function () {

    describe('Date Formatter', function () {
      it('adds leading zeros to minutes', function () {
        var model;

        model = new Log.Model({
                  date: 'Sat, 12 Apr 2014 15:04:01 GMT',
                  log_level: 'info',
                  pid: '123',
                  args: 'ente ente'
                });
        // timezones with daylightsaving in JS are hard
        // and we use the current local time here
        // so do not test for hours and the exact day
        assert.ok(/Apr \d\d \d\d:04:01/.test(model.date()));
      });
    });
  });
});
