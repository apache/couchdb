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

import copy
import mango
import unittest

DOCS = [
    {"_id": "100", "name": "Jimi", "location": "AUS", "user_id": 1, "same": "value"},
    {"_id": "200", "name": "Eddie", "location": "BRA", "user_id": 2, "same": "value"},
    {"_id": "300", "name": "Harry", "location": "CAN", "user_id": 3, "same": "value"},
    {"_id": "400", "name": "Eddie", "location": "DEN", "user_id": 4, "same": "value"},
    {"_id": "500", "name": "Jones", "location": "ETH", "user_id": 5, "same": "value"},
    {
        "_id": "600",
        "name": "Winnifried",
        "location": "FRA",
        "user_id": 6,
        "same": "value",
    },
    {"_id": "700", "name": "Marilyn", "location": "GHA", "user_id": 7, "same": "value"},
    {"_id": "800", "name": "Sandra", "location": "ZAR", "user_id": 8, "same": "value"},
]

oldschoolnoselectorddoc = {
    "_id": "_design/oldschoolnoselector",
    "language": "query",
    "views": {
        "oldschoolnoselector": {
            "map": {"fields": {"location": "asc"}},
            "reduce": "_count",
            "options": {"def": {"fields": ["location"]}},
        }
    },
}

oldschoolddoc = {
    "_id": "_design/oldschool",
    "language": "query",
    "views": {
        "oldschool": {
            "map": {
                "fields": {"location": "asc"},
                "selector": {"location": {"$gte": "FRA"}},
            },
            "reduce": "_count",
            "options": {"def": {"fields": ["location"]}},
        }
    },
}

oldschoolddoctext = {
    "_id": "_design/oldschooltext",
    "language": "query",
    "indexes": {
        "oldschooltext": {
            "index": {
                "default_analyzer": "keyword",
                "default_field": {},
                "selector": {"location": {"$gte": "FRA"}},
                "fields": [{"name": "location", "type": "string"}],
                "index_array_lengths": True,
            },
            "analyzer": {
                "name": "perfield",
                "default": "keyword",
                "fields": {"$default": "standard"},
            },
        }
    },
}


class IndexSelectorJson(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_saves_partial_filter_selector_in_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], partial_filter_selector=selector)
        indexes = self.db.list_indexes()
        self.assertEqual(indexes[1]["def"]["partial_filter_selector"], selector)

    def test_partial_filter_only_in_return_if_not_default(self):
        self.db.create_index(["location"])
        index = self.db.list_indexes()[1]
        self.assertEqual("partial_filter_selector" in index["def"], False)

    def test_saves_selector_in_index_throws(self):
        selector = {"location": {"$gte": "FRA"}}
        try:
            self.db.create_index(["location"], selector=selector)
        except Exception as e:
            assert e.response.status_code == 400
        else:
            raise AssertionError("bad index creation")

    def test_uses_partial_index_for_query_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(
            ["location"],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        resp = self.db.find(selector, explain=True, use_index="Selected")
        self.assertEqual(resp["index"]["name"], "Selected")
        docs = self.db.find(selector, use_index="Selected")
        self.assertEqual(len(docs), 3)

    def test_uses_partial_index_with_different_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        selector2 = {"location": {"$gte": "A"}}
        self.db.create_index(
            ["location"],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        resp = self.db.find(selector2, explain=True, use_index="Selected")
        self.assertEqual(resp["index"]["name"], "Selected")
        docs = self.db.find(selector2, use_index="Selected")
        self.assertEqual(len(docs), 3)

    def test_doesnot_use_selector_when_not_specified(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(
            ["location"],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    def test_doesnot_use_selector_when_not_specified_with_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(
            ["location"],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        self.db.create_index(["location"], name="NotSelected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "NotSelected")

    def test_old_selector_with_no_selector_still_supported(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.save_doc(oldschoolnoselectorddoc)
        self.db.wait_for_built_indexes()
        resp = self.db.find(selector, explain=True, use_index="oldschoolnoselector")
        self.assertEqual(resp["index"]["name"], "oldschoolnoselector")
        docs = self.db.find(selector, use_index="oldschoolnoselector")
        self.assertEqual(len(docs), 3)

    def test_old_selector_still_supported(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.save_doc(oldschoolddoc)
        self.db.wait_for_built_indexes()
        resp = self.db.find(selector, explain=True, use_index="oldschool")
        self.assertEqual(resp["index"]["name"], "oldschool")
        docs = self.db.find(selector, use_index="oldschool")
        self.assertEqual(len(docs), 3)

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_saves_partialfilterselector_in_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}],
            partial_filter_selector=selector,
        )
        indexes = self.db.list_indexes()
        self.assertEqual(indexes[1]["def"]["partial_filter_selector"], selector)

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_uses_partial_index_for_query_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        resp = self.db.find(selector, explain=True, use_index="Selected")
        self.assertEqual(resp["index"]["name"], "Selected")
        docs = self.db.find(selector, use_index="Selected", fields=["_id", "location"])
        self.assertEqual(len(docs), 3)

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_uses_partial_index_with_different_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        selector2 = {"location": {"$gte": "A"}}
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        resp = self.db.find(selector2, explain=True, use_index="Selected")
        self.assertEqual(resp["index"]["name"], "Selected")
        docs = self.db.find(selector2, use_index="Selected")
        self.assertEqual(len(docs), 3)

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_doesnot_use_selector_when_not_specified(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_doesnot_use_selector_when_not_specified_with_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}],
            partial_filter_selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}], name="NotSelected"
        )
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "NotSelected")

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_old_selector_still_supported(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.save_doc(oldschoolddoctext)
        resp = self.db.find(selector, explain=True, use_index="oldschooltext")
        self.assertEqual(resp["index"]["name"], "oldschooltext")
        docs = self.db.find(selector, use_index="oldschooltext")
        self.assertEqual(len(docs), 3)

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_old_selector_still_supported_via_api(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_text_index(
            fields=[{"name": "location", "type": "string"}],
            selector=selector,
            ddoc="Selected",
            name="Selected",
        )
        docs = self.db.find({"location": {"$exists": True}}, use_index="Selected")
        self.assertEqual(len(docs), 3)

    @unittest.skipUnless(mango.has_text_service(), "requires text service")
    def test_text_partial_filter_only_in_return_if_not_default(self):
        self.db.create_text_index(fields=[{"name": "location", "type": "string"}])
        index = self.db.list_indexes()[1]
        self.assertEqual("partial_filter_selector" in index["def"], False)
