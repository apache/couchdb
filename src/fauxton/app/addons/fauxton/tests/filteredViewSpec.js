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
       'app',
       'addons/fauxton/components',
       'testUtils'
], function (app, Components, testUtils) {
  var assert = testUtils.assert;

  describe('FilteredView', function () {
    var filteredView;


    beforeEach(function () {
      filteredView = new Components.FilteredView();
    });

    afterEach(function () {
    });

    it('should be case insensitive', function () {
      filteredView.filters = ['ente'];
      var res = filteredView.createFilteredData([
        {id: 'LALA', bar: 'ENTE'},
        {id: '1', bar: '1', deleted: true},
        {id: '2', bar: '2'}
      ]);
      assert.equal(res.length, 1);
    });
  });
});
