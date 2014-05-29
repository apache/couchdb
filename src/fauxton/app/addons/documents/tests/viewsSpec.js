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
        'addons/documents/views',
        'addons/databases/base',
        'testUtils'
], function (Views, Databases, testUtils) {
  var assert = testUtils.assert;

  describe('DocumentsViews', function () {
    it('should load', function () {
      assert.equal(typeof Views.Views.Changes, 'function');
    });
  });

  describe('Changes', function () {
    var filteredView;
    beforeEach(function () {
      var database = new Databases.Model({id: 'bla'});
      database.buildChanges({descending: 'true', limit: '100', include_docs: 'true'} );
      filteredView = new Views.Views.Changes({
        model: database
      });
    });

    it('filter false in case of deleted documents in the changes feed', function () {
      filteredView.filters = [false];
      var res = filteredView.createFilteredData([
        {id: 'LALA', bar: 'ENTE'},
        {id: '1', bar: '1', deleted: true},
        {id: '2', bar: '2'}
      ]);

      assert.equal(res.length, 2);
    });
  });
});
