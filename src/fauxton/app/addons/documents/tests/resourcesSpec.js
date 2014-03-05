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
       'addons/documents/resources',
      'testUtils'
], function (Models, testUtils) {
  var assert = testUtils.assert;

  describe('IndexCollection', function () {
    var collection;
    beforeEach(function () {
      collection = new Models.IndexCollection([{
        id:'myId1',
        doc: 'num1'
      },
      {
        id:'myId2',
        doc: 'num2'
      }], {
        database: {id: 'databaseId', safeID: function () { return this.id; }},
        design: '_design/myDoc'
      });

    });

  });

  describe('AllDocs', function () {
    var collection;
    beforeEach(function () {
      collection = new Models.AllDocs([{
        _id:'myId1',
        doc: 'num1'
      },
      {
        _id:'myId2',
        doc: 'num2'
      }], {
        database: {id: 'databaseId', safeID: function () { return this.id; }},
        params: {limit: 20}
      });

    });

  });

});

