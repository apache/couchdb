# Licensed under the Apache License, Version 2.0 (the "License"); you may not
# use this file except in compliance with the License. You may obtain a copy of
# the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
# WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
# License for the specific language governing permissions and limitations under
# the License.

import mango
import user_docs
import unittest


class ExplainIndexJSONTests(mango.UserDocsTests):
    def test_basic(self):
        resp = self.db.find({"age": 23}, explain=True)
        candidate_indexes = [
            {
                "index": {
                    "ddoc": "_design/user_id",
                    "name": "user_id",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"user_id": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/twitter",
                    "name": "twitter",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"twitter": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/ordered",
                    "name": "ordered",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"ordered": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/name",
                    "name": "name",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"name.last": "asc"}, {"name.first": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/manager",
                    "name": "manager",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"manager": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/location",
                    "name": "location",
                    "type": "json",
                    "partitioned": False,
                    "def": {
                        "fields": [
                            {"location.state": "asc"},
                            {"location.city": "asc"},
                            {"location.address.street": "asc"},
                            {"location.address.number": "asc"},
                        ]
                    },
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/favorites_3",
                    "name": "favorites_3",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"favorites.3": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/favorites",
                    "name": "favorites",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"favorites": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/company_and_manager",
                    "name": "company_and_manager",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"company": "asc"}, {"manager": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": None,
                    "name": "_all_docs",
                    "type": "special",
                    "def": {"fields": [{"_id": "asc"}]},
                },
                "analysis": {
                    "usable": True,
                    "reasons": [{"name": "unfavored_type"}],
                    "ranking": 1,
                    "covering": None,
                },
            },
        ]
        with self.subTest(subject="index_candidates"):
            assert resp["index_candidates"] == candidate_indexes
        with self.subTest(subject="selector_hints"):
            selector_hints = resp["selector_hints"][0]
            assert selector_hints == {
                "type": "json",
                "indexable_fields": ["age"],
                "unindexable_fields": [],
            }


class ExplainIndexTestsNoIndexes(mango.UserDocsTestsNoIndexes):
    def test_basic(self):
        resp = self.db.find({"age": 23}, explain=True)
        assert resp["index_candidates"] == []
        assert resp["index"] != None
        assert resp["partitioned"] == False

    def test_no_usable_index(self):
        resp = self.db.find({"age": 23}, sort=["company"], explain=True)
        candidate_indexes = [
            {
                "index": {
                    "ddoc": None,
                    "name": "_all_docs",
                    "type": "special",
                    "def": {"fields": [{"_id": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 1,
                    "covering": None,
                },
            }
        ]
        assert resp["index_candidates"] == candidate_indexes
        assert resp["index"] == None
        assert resp["partitioned"] == False


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class ExplainIndexTextTests(mango.UserDocsTextTests):
    def test_basic(self):
        resp = self.db.find({"age": 23}, explain=True)
        candidate_indexes = [
            {
                "index": {
                    "ddoc": None,
                    "name": "_all_docs",
                    "type": "special",
                    "def": {"fields": [{"_id": "asc"}]},
                },
                "analysis": {
                    "usable": True,
                    "reasons": [{"name": "unfavored_type"}],
                    "ranking": 1,
                    "covering": None,
                },
            }
        ]
        with self.subTest(subject="index_candidates"):
            assert resp["index_candidates"] == candidate_indexes
        with self.subTest(subject="selector_hints"):
            selector_hints = resp["selector_hints"]
            assert selector_hints == [
                {"type": "json", "indexable_fields": ["age"], "unindexable_fields": []},
                {"type": "text", "indexable_fields": ["age"], "unindexable_fields": []},
            ]


@unittest.skipUnless(mango.has_text_service(), "requires text service")
class ExplainIndexMixedTests(mango.DbPerClass):
    @classmethod
    def setUpClass(klass):
        super(ExplainIndexMixedTests, klass).setUpClass()
        user_docs.setup(klass.db)
        klass.db.create_text_index()

    def test_basic(self):
        resp = self.db.find({"age": 23}, explain=True)
        candidate_indexes = [
            {
                "index": {
                    "ddoc": "_design/user_id",
                    "name": "user_id",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"user_id": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/twitter",
                    "name": "twitter",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"twitter": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/ordered",
                    "name": "ordered",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"ordered": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/name",
                    "name": "name",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"name.last": "asc"}, {"name.first": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/manager",
                    "name": "manager",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"manager": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/location",
                    "name": "location",
                    "type": "json",
                    "partitioned": False,
                    "def": {
                        "fields": [
                            {"location.state": "asc"},
                            {"location.city": "asc"},
                            {"location.address.street": "asc"},
                            {"location.address.number": "asc"},
                        ]
                    },
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/favorites_3",
                    "name": "favorites_3",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"favorites.3": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/favorites",
                    "name": "favorites",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"favorites": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/company_and_manager",
                    "name": "company_and_manager",
                    "type": "json",
                    "partitioned": False,
                    "def": {"fields": [{"company": "asc"}, {"manager": "asc"}]},
                },
                "analysis": {
                    "usable": False,
                    "reasons": [{"name": "field_mismatch"}],
                    "ranking": 2,
                    "covering": False,
                },
            },
            {
                "index": {
                    "ddoc": "_design/cedc01a027213706d7260b5e5b73c70b9233743a",
                    "name": "cedc01a027213706d7260b5e5b73c70b9233743a",
                    "type": "text",
                    "partitioned": False,
                    "def": {
                        "default_analyzer": "keyword",
                        "default_field": {},
                        "selector": {},
                        "fields": [],
                        "index_array_lengths": True,
                    },
                },
                "analysis": {
                    "usable": True,
                    "reasons": [{"name": "unfavored_type"}],
                    "ranking": 1,
                    "covering": None,
                },
            },
            {
                "index": {
                    "ddoc": None,
                    "name": "_all_docs",
                    "type": "special",
                    "def": {"fields": [{"_id": "asc"}]},
                },
                "analysis": {
                    "usable": True,
                    "reasons": [{"name": "unfavored_type"}],
                    "ranking": 1,
                    "covering": None,
                },
            },
        ]
        assert resp["index_candidates"] == candidate_indexes


class ExplainIndexTestsPartitionedNoIndexes(mango.PartitionedUserDocsTestsNoIndexes):
    def test_basic(self):
        resp = self.db.find({"age": 23}, explain=True)
        assert resp["index_candidates"] == []
        assert resp["index"] != None
        assert resp["partitioned"] == True
