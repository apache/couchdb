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

  describe('QueryParams', function() {
    describe('parse', function() {
      it('should not parse arbitrary parameters', function() {
        var params = {"foo":"[1]]"};
        var result = Models.QueryParams.parse(params);

        assert.deepEqual(result, params);
      });

      it('parses startkey, endkey', function() {
        var params = {
          "startkey":"[\"a\",\"b\"]",
          "endkey":"[\"c\",\"d\"]"
        };
        var result = Models.QueryParams.parse(params);

        assert.deepEqual(result, {
          "startkey":["a","b"],
          "endkey":["c","d"]
        });
      });

      it('parses key', function() {
        var params = {
          "key":"[1,2]"
        };
        var result = Models.QueryParams.parse(params);

        assert.deepEqual(result, {"key":[1,2]});
      });

      it('does not modify input', function() {
        var params = {
          "key":"[\"a\",\"b\"]"
        };
        var clone = _.clone(params);
        var result = Models.QueryParams.parse(params);

        assert.deepEqual(params, clone);
      });
    });

    describe('stringify', function() {
      it('should not stringify arbitrary parameters', function() {
        var params = {"foo":[1,2,3]};
        var result = Models.QueryParams.stringify(params);

        assert.deepEqual(result, params);
      });

      it('stringifies startkey, endkey', function() {
        var params = {
          "startkey":["a","b"],
          "endkey":["c","d"]
        };

        var result = Models.QueryParams.stringify(params);

        assert.deepEqual(result, {
          "startkey":"[\"a\",\"b\"]",
          "endkey":"[\"c\",\"d\"]"
        });
      });

      it('stringifies key', function() {
        var params = {"key":["a","b"]};
        var result = Models.QueryParams.stringify(params);

        assert.deepEqual(result, { "key":"[\"a\",\"b\"]" });
      });

      it('does not modify input', function() {
        var params = {"key":["a","b"]};
        var clone = _.clone(params);
        var result = Models.QueryParams.stringify(params);

        assert.deepEqual(params, clone);
      });

      it('is symmetrical with parse', function() {
        var params = {
          "startkey":["a","b"],
          "endkey":["c","d"],
          "foo": "[1,2]",
          "bar": "abc"
        };

        var clone = _.clone(params);
        var json = Models.QueryParams.stringify(params);
        var result = Models.QueryParams.parse(json);

        assert.deepEqual(result, clone);
      });
    });
  });

  describe('Bulk Delete', function () {
    var databaseId = 'ente',
        collection,
        values;

    values = [{
      _id: '1',
      _rev: '1234561',
      _deleted: true
    },
    {
      _id: '2',
      _rev: '1234562',
      _deleted: true
    },
    {
      _id: '3',
      _rev: '1234563',
      _deleted: true
    }];

    beforeEach(function () {
      collection = new Models.BulkDeleteDocCollection(values, {
        databaseId: databaseId
      });
    });

    it("contains the models", function () {
      collection = new Models.BulkDeleteDocCollection(values, {
        databaseId: databaseId
      });

      assert.equal(collection.length, 3);
    });

    it("clears the memory if no errors happened", function () {
      collection.handleResponse([
        {"ok":true,"id":"1","rev":"10-72cd2edbcc0d197ce96188a229a7af01"},
        {"ok":true,"id":"2","rev":"6-da537822b9672a4b2f42adb1be04a5b1"}
      ]);

      assert.equal(collection.length, 1);
    });

    it("triggers a removed event with all ids", function () {
      collection.listenToOnce(collection, 'removed', function (ids) {
        assert.deepEqual(ids, ['Deferred', 'DeskSet']);
      });

      collection.handleResponse([
        {"ok":true,"id":"Deferred","rev":"10-72cd2edbcc0d197ce96188a229a7af01"},
        {"ok":true,"id":"DeskSet","rev":"6-da537822b9672a4b2f42adb1be04a5b1"}
      ]);
    });

    it("triggers a error event with all errored ids", function () {
      collection.listenToOnce(collection, 'error', function (ids) {
        assert.deepEqual(ids, ['Deferred']);
      });
      collection.handleResponse([
        {"error":"confclict","id":"Deferred","rev":"10-72cd2edbcc0d197ce96188a229a7af01"},
        {"ok":true,"id":"DeskSet","rev":"6-da537822b9672a4b2f42adb1be04a5b1"}
      ]);
    });

    it("removes successfull deleted from the collection but keeps one with errors", function () {
      collection.handleResponse([
        {"error":"confclict","id":"1","rev":"10-72cd2edbcc0d197ce96188a229a7af01"},
        {"ok":true,"id":"2","rev":"6-da537822b9672a4b2f42adb1be04a5b1"},
        {"error":"conflict","id":"3","rev":"6-da537822b9672a4b2f42adb1be04a5b1"}
      ]);
      assert.ok(collection.get('1'));
      assert.ok(collection.get('3'));
      assert.notOk(collection.get('2'));
    });
  });
});
