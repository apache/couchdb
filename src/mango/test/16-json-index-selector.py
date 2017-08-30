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
    {
        "_id": "100",
        "name": "Jimi",
        "location": "AUS",
        "user_id": 1,
        "same": "value"
    },
    {
        "_id": "200",
        "name": "Eddie",
        "location": "BRA",
        "user_id": 2,
        "same": "value"
    },
    {
        "_id": "300",
        "name": "Harry",
        "location": "CAN",
        "user_id":3,
        "same": "value"
    },
    {
        "_id": "400",
        "name": "Eddie",
        "location": "DEN",
        "user_id":4,
        "same": "value"
    },
    {
        "_id": "500",
        "name": "Jones",
        "location": "ETH",
        "user_id":5,
        "same": "value"
    },
    {
        "_id": "600",
        "name": "Winnifried",
        "location": "FRA",
        "user_id":6,
        "same": "value"
    },
    {
        "_id": "700",
        "name": "Marilyn",
        "location": "GHA",
        "user_id":7,
        "same": "value"
    },
    {
        "_id": "800",
        "name": "Sandra",
        "location": "ZAR",
        "user_id":8,
        "same": "value"
    },
]

class IndexSelectorJson(mango.DbPerClass):
    def setUp(self):
        self.db.recreate()
        self.db.save_docs(copy.deepcopy(DOCS))

    def test_saves_selector_in_index(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector)
        indexes = self.db.list_indexes()
        self.assertEqual(indexes[1]["def"]["selector"], selector)

    def test_uses_partial_index_for_query_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    def test_uses_partial_index_for_query_multiple_fields(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"location":{"$gte": "FRA"}, "user_id": 7}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    def test_uses_partial_index_for_query_selector_multiple_fields(self):
        selector = {"location": {"$gte": "FRA"}, "user_id": 7}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    def test_uses_partial_index_for_query_selector_multiple_fields_diff_order(self):
        selector = {"location": {"$gte": "FRA"}, "user_id": 7}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"user_id": 7, "location": {"$gte": "FRA"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    def test_does_not_use_partial_index_for_or_query(self):
        selector = {"location": {"$gte": "FRA"}, "user_id": 7}
        self.db.create_index(["location"], selector=selector, name="Selected")
        self.db.create_index(["location"], name="NotSelected")
        resp = self.db.find({"$or":[selector, {"name": "Sandra"}]}, explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    def test_does_not_use_partial_index_for_disjoint_query_selector(self):
        selector = {"location": {"$gte": "FRA"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find({"location":{"$eq": "ZAR"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    def test_prefers_partial_index_over_global_index(self):
        selector = {"location": {"$gte": "ZAR"}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        self.db.create_index(["location"], name="NotSelected")
        resp = self.db.find({"location":{"$gte": "ZAR"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    @unittest.skip("aspirational - currently fails")
    def test_prefers_most_selective_partial_index(self):
        self.db.create_index(["location"], selector={"location": {"$gte": "ZAR"}},
                             name="Selected location")
        self.db.create_index(["location"], selector={"location": {"$gte": "ZAR"}, "user_id": 7},
                             name="Selected location and user")
        resp = self.db.find({"location": {"$gte": "ZAR"}, "user_id": 7}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected location and user")

    def test_does_not_use_partial_index_that_is_too_selective(self):
        self.db.create_index(["location"], selector={"location": {"$gte": "ZAR"}},
                             name="Selected location")
        self.db.create_index(["location"], selector={"location": {"$gte": "ZAR"}, "user_id": 7},
                             name="Selected location and user")
        resp = self.db.find({"location": {"$gte": "ZAR"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected location")

    def test_does_not_use_partial_index_with_invalid_query_sort(self):
        self.db.create_index(["location"], selector={"location": {"$gte": "ZAR"}},
                             name="Selected")
        resp = self.db.find({"location": {"$gte": "ZAR"}}, sort=[{"location":"desc"}], explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    def test_does_not_use_partial_index_with_invalid_index_sort(self):
        self.db.create_index([{"location":"desc"}], selector={"location": {"$gte": "ZAR"}},
                             name="Selected")
        resp = self.db.find({"location": {"$gte": "ZAR"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "_all_docs")

    @unittest.skip("aspirational - currently fails")
    def test_uses_overlapping_but_not_exact_partial_index(self):
        self.db.create_index(["location"], selector={"location": {"$exists": True}},
                             name="Selected")
        resp = self.db.find({"location": {"$gte": "ZAR"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    @unittest.skip("aspirational - currently fails")
    def test_uses_partial_index_for_ne(self):
        self.db.create_index(["location"], selector={"location": {"$ne": "ZAR"}},
                             name="Selected")
        resp = self.db.find({"location": {"$ne": "ZAR"}}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    def test_uses_partial_index_for_ne_non_indexed_field(self):
        self.db.create_index(["user_id"], selector={"location": {"$ne": "ZAR"}},
                             name="Selected")
        resp = self.db.find({"location": {"$ne": "ZAR"}, "user_id": 7}, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    @unittest.skip("aspirational - currently fails")
    def test_partial_index_with_combination_selector(self):
        selector = {"$or": [
            {"location": "ZAR"},
            {"location": "BRA"}]}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    @unittest.skip("aspirational - currently fails")
    def test_uses_partial_index_for_ne_null(self):
        selector = {"location": {"$ne": None}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    @unittest.skip("aspirational - currently fails")
    def test_uses_partial_index_for_exists(self):
        selector = {"location": {"$exists": True}}
        self.db.create_index(["location"], selector=selector, name="Selected")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "Selected")

    @unittest.skip("aspirational - currently fails")
    def test_uses_global_index_for_exists(self):
        selector = {"location": {"$exists": True}}
        self.db.create_index(["location"], name="Global")
        resp = self.db.find(selector, explain=True)
        self.assertEqual(resp["index"]["name"], "Global")
