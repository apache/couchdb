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
       'modules/documents/resources',
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
        database: {id: 'databaseId'},
        design: '_design/myDoc'
      });

    });

    it('Should return urlNext', function () {
      var url = collection.urlNextPage(20);

      assert.equal(url, 'database/databaseId/_design/myDoc/_view/?limit=20&reduce=false&startkey_docid=%22myId2%22&startkey=%22myId2%22');

    });

    it('Should return urlPrevious', function () {
      var url = collection.urlPreviousPage(20, 'myId1');

      assert.equal(url, 'database/databaseId/_design/myDoc/_view/?limit=20&reduce=false&startkey_docid=%22myId1%22&startkey=%22myId1%22');

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
        database: {id: 'databaseId'},
        params: {limit: 20}
      });

    });

    it('Should return urlNext', function () {
      var url = collection.urlNextPage(20);

      assert.equal(url, 'database/databaseId/_all_docs?limit=20&startkey_docid=%22myId2%22&startkey=%22myId2%22');

    });

     it('Should return urlPrevious', function () {
      var url = collection.urlPreviousPage(20, 'myId1');
      assert.equal(url, 'database/databaseId/_all_docs?limit=20&startkey_docid=%22myId1%22&startkey=%22myId1%22');
    });


  });

});

