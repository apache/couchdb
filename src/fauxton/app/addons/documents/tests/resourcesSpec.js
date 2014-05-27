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
});

